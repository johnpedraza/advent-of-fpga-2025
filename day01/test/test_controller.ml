(* Test Day 1 Controller 
 *
 * Based on input ASCII character, determine if it's a digit or a newline,
 * and update turn direction if char is 'L' or 'R'. Assert a "done" signal
 * when 0x4 (End of Transmission) is encountered.
 *
*)

open! Core
open! Hardcaml
open! Hardcaml_waveterm
open! Hardcaml_test_harness
module Controller = Day01.Controller
module Harness = Cyclesim_harness.Make (Controller.I) (Controller.O)

let testbench (sim : Harness.Sim.t) =
  let inputs = Cyclesim.inputs sim in
  let outputs = Cyclesim.outputs sim in
  let cycle ?n () = Cyclesim.cycle ?n sim in
  let reset () = Cyclesim.reset sim in
  reset ();
  cycle ();
  let send_ascii chars =
    List.iter chars ~f:(fun c ->
      inputs.ascii.valid := Bits.vdd;
      inputs.ascii.value := Bits.of_unsigned_int ~width:8 c;
      cycle ();
      Stdio.printf "direction='%s' ; " (Bits.to_string !(outputs.direction));
      Stdio.printf "is_digit='%s' ; " (Bits.to_string !(outputs.is_digit));
      Stdio.printf "is_newline='%s' ; " (Bits.to_string !(outputs.is_newline));
      Stdio.printf "done_='%s'\n" (Bits.to_string !(outputs.done_)))
  in
  send_ascii [ 0x52; 0x30; 0x35; 0x39; 0x0A; 0x4C; 0x35; 0x37; 0x0A; 0x04 ]
;;

(* Unused scope *)
let create (_scope : Scope.t) (i : Signal.t Controller.I.t) = Controller.create i

let%expect_test "test controller with waveform" =
  let display_rules =
    [ Display_rule.port_name_matches
        ~wave_format:(Bit_or Unsigned_int)
        (Re.Glob.glob "*" |> Re.compile)
      (* Select signals to display in waveform *)
    ]
  in
  Harness.run_advanced
    ~create
    ~trace:`All_named
    ~print_waves_after_test:(fun waves ->
      Waveform.print
        ~display_rules
        ~signals_width:30
        ~display_width:80
        ~wave_width:1
        waves)
    testbench;
  [%expect
    {|
    direction='1' ; is_digit='0' ; is_newline='0' ; done_='0'
    direction='1' ; is_digit='1' ; is_newline='0' ; done_='0'
    direction='1' ; is_digit='1' ; is_newline='0' ; done_='0'
    direction='1' ; is_digit='1' ; is_newline='0' ; done_='0'
    direction='1' ; is_digit='0' ; is_newline='1' ; done_='0'
    direction='0' ; is_digit='0' ; is_newline='0' ; done_='0'
    direction='0' ; is_digit='1' ; is_newline='0' ; done_='0'
    direction='0' ; is_digit='1' ; is_newline='0' ; done_='0'
    direction='0' ; is_digit='0' ; is_newline='1' ; done_='0'
    direction='0' ; is_digit='0' ; is_newline='0' ; done_='1'
    ┌Signals─────────────────────┐┌Waves───────────────────────────────────────────┐
    │ascii$valid                 ││        ┌───────────────────────────────────────│
    │                            ││────────┘                                       │
    │                            ││────────┬───┬───┬───┬───┬───┬───┬───┬───┬───┬───│
    │ascii$value                 ││ 0      │82 │48 │53 │57 │10 │76 │53 │55 │10 │4  │
    │                            ││────────┴───┴───┴───┴───┴───┴───┴───┴───┴───┴───│
    │clock                       ││┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ ┌─┐ │
    │                            ││  └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─┘ └─│
    │reset_n                     ││                                                │
    │                            ││────────────────────────────────────────────────│
    │direction                   ││            ┌───────────────────┐               │
    │                            ││────────────┘                   └───────────────│
    │done_                       ││                                            ┌───│
    │                            ││────────────────────────────────────────────┘   │
    │is_digit                    ││            ┌───────────┐       ┌───────┐       │
    │                            ││────────────┘           └───────┘       └───────│
    │is_newline                  ││                        ┌───┐           ┌───┐   │
    │                            ││────────────────────────┘   └───────────┘   └───│
    └────────────────────────────┘└────────────────────────────────────────────────┘
    |}]
;;
