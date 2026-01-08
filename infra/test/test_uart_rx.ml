open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Uart_rx = Aof_infra.Uart_rx
module Harness = Cyclesim_harness.Make (Uart_rx.I) (Uart_rx.O)

let testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  (* Given a data byte, generate a UART frame *)
  let uart_frame byte =
    let bits = List.init 8 ~f:(fun x -> (byte lsr x) land 1) in
    [ 0 ] @ bits @ [ 1 ]
  in
  (* Transmit bits at baud rate *)
  let drive_bits ~full_period bits =
    List.iter bits ~f:(fun b ->
      for _ = 1 to full_period do
        cycle ()
      done;
      inputs.rx := Bits.of_unsigned_int ~width:1 b)
  in
  (* Wait for a valid bit *)
  let wait_for_valid ~timeout =
    let rec loop n =
      if n = 0 then failwith "Timed out waiting for valid";
      cycle ();
      if Bits.to_unsigned_int !(outputs.rx_byte.valid) = 1 then () else loop (n - 1)
    in
    loop timeout
  in
  (* reset *)
  inputs.reset_n := Bits.gnd;
  cycle ~n:10 ();
  inputs.reset_n := Bits.vdd;
  cycle ();
  (* idle high for some time *)
  let clock_freq = 100_000_000 in
  let baud_rate = 115_200 in
  let full_period = clock_freq / baud_rate in
  inputs.rx := Bits.vdd;
  for _ = 1 to full_period * 50 do
    cycle ()
  done;
  (* send frame *)
  let bits = uart_frame 0x55 in
  drive_bits ~full_period bits;
  (* should see valid pulse shortly after *)
  wait_for_valid ~timeout:(full_period * 2);
  let packet_value = Bits.to_unsigned_int !(outputs.rx_byte.value) in
  Stdio.printf
    "value=0x%02X valid=%d\n"
    packet_value
    (Bits.to_unsigned_int !(outputs.rx_byte.valid))
;;

let%expect_test "simple test" =
  Harness.run_advanced ~create:Uart_rx.hierarchical testbench;
  [%expect {| value=0x55 valid=1 |}]
;;
