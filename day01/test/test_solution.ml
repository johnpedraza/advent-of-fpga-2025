(* Test Day 1 Solution *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Solution = Day01.Solution
module Harness = Cyclesim_harness.Make (Solution.I) (Solution.O)

let testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let reset () = Cyclesim.reset sim in
  reset ();
  cycle ();
  (* Present a valid character on every clock cycle. *)
  let feed_char c =
    inputs.ascii.value := Bits.of_char c;
    inputs.ascii.valid := Bits.vdd;
    cycle ();
    inputs.ascii.valid := Bits.gnd;
    cycle ();
  in
  let input_text = In_channel.read_all "../input/example.txt" in
  let input_chars = String.to_list input_text in
  List.iter input_chars ~f:(fun x -> feed_char x);
  inputs.ascii.value := Bits.of_unsigned_int ~width:8 0x04;
  inputs.ascii.valid := Bits.vdd;
  cycle ();
  inputs.ascii.valid := Bits.gnd;
  cycle ();
  Stdio.printf "Part 1: %d\n" (Bits.to_unsigned_int !(outputs.result_part1));
  Stdio.printf "Part 2: %d\n" (Bits.to_unsigned_int !(outputs.result_part2));
;;

let%expect_test "Test Day 01 Solution" =
  Harness.run_advanced ~create:Solution.hierarchical testbench;
  [%expect {|
    Part 1: 3
    Part 2: 6
    |}]
;;
