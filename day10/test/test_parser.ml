(* Test Day 10 Parser *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Parser = Day10.Parser
module Harness = Cyclesim_harness.Make (Parser.I) (Parser.O)

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
      inputs.ascii_char.valid := Bits.gnd;
      if Bits.to_bool !(outputs.valid)
      then (
        Stdio.printf "desired_lights: %s\n" (Bits.to_string !(outputs.desired_lights));
        for i = 0 to 12 do
          Stdio.printf "button %d: %s\n" i (Bits.to_string !(outputs.buttons.(i)))
        done);
      cycle ~n:3 ())
  in
  let input_text = In_channel.read_all "../input/example.txt" in
  let input_chars = String.to_list input_text in
  send_ascii input_chars;
  inputs.ascii_char.valid := Bits.vdd;
  inputs.ascii_char.value := Bits.of_unsigned_int ~width:8 0x04;
  cycle ();
  inputs.ascii_char.valid := Bits.gnd;
  cycle ~n:3 ();
;;

let%expect_test "Test Day 10 Parser" =
  Harness.run_advanced ~create:Parser.hierarchical testbench;
  [%expect
    {|
    desired_lights: 0110000000
    button 0: 0001000000
    button 1: 0101000000
    button 2: 0010000000
    button 3: 0011000000
    button 4: 1010000000
    button 5: 1100000000
    button 6: 0000000000
    button 7: 0000000000
    button 8: 0000000000
    button 9: 0000000000
    button 10: 0000000000
    button 11: 0000000000
    button 12: 0000000000
    desired_lights: 0001000000
    button 0: 1011100000
    button 1: 0011000000
    button 2: 1000100000
    button 3: 1110000000
    button 4: 0111100000
    button 5: 0000000000
    button 6: 0000000000
    button 7: 0000000000
    button 8: 0000000000
    button 9: 0000000000
    button 10: 0000000000
    button 11: 0000000000
    button 12: 0000000000
    desired_lights: 0111010000
    button 0: 1111100000
    button 1: 1001100000
    button 2: 1110110000
    button 3: 0110000000
    button 4: 0000000000
    button 5: 0000000000
    button 6: 0000000000
    button 7: 0000000000
    button 8: 0000000000
    button 9: 0000000000
    button 10: 0000000000
    button 11: 0000000000
    button 12: 0000000000
    |}]
;;
