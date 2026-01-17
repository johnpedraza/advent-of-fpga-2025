(* Test Day 10 Solution *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Solution = Day10.Solution
module Harness = Cyclesim_harness.Make (Solution.I) (Solution.O)

let testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let reset () = Cyclesim.reset sim in
  reset ();
  cycle ();
  let send_ascii chars =
    List.iter chars ~f:(fun c ->
      inputs.ascii_char.valid := Bits.vdd;
      inputs.ascii_char.value := Bits.of_char c;
      cycle ();
      inputs.ascii_char.valid := Bits.gnd)
  in
  let input_text = In_channel.read_all "../input/example.txt" in
  let input_chars = String.to_list input_text in
  send_ascii input_chars;
  inputs.ascii_char.valid := Bits.vdd;
  inputs.ascii_char.value := Bits.of_unsigned_int ~width:8 0x04;
  (* EOT *)
  cycle ();
  inputs.ascii_char.valid := Bits.gnd;
  let wait_for_done ~timeout =
    let rec loop n =
      if n = 0
      then failwith "Timed out waiting for done"
      else (
        cycle ();
        if Bits.to_bool !(outputs.valid)
        then (
          Stdio.printf
            "Part 1 Result: %d\n"
            (Bits.to_unsigned_int !(outputs.fewest_button_presses));
          Stdio.printf "valid: %s\n" (Bits.to_string !(outputs.valid));
          Stdio.printf
            "Num cycles after all input received: %d\n"
            (Bits.to_unsigned_int !(outputs.perf_counter)))
        else loop (n - 1))
    in
    loop timeout
  in
  wait_for_done ~timeout:100000
;;

let%expect_test "Test Day 10 Solution" =
  Harness.run_advanced ~create:Solution.hierarchical testbench;
  [%expect
    {|
    Part 1 Result: 7
    valid: 1
    Num cycles after all input received: 8190
    |}]
;;
