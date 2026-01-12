(* Test Day 3 Solution *)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Solution = Day03.Solution
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
  cycle ~n:10 ();
  Stdio.printf "Part 2 Example: %d\n" (Bits.to_unsigned_int !(outputs.total_joltage_part1))
;;

let%expect_test "Test Day 03 Solution" =
  Harness.run_advanced ~create:Solution.hierarchical testbench;
  [%expect {| Part 2 Example: 3121910778619 |}]
;;

(*
   let%expect_test "Test solution with waveform" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "*" |> Re.compile)
    ]
  in
  Harness.run_advanced
    ~create:Solution.hierarchical
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:80
        ~wave_width:1
        waves)
    testbench;
*)
