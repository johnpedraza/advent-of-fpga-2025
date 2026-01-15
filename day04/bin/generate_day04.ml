(* Generate RTL (Verilog) for Day 4 solution.
 * Includes infrastructure like UART receiver and 7-segment display logic. *)

open! Core
open! Hardcaml
open Day04

let generate_day04_rtl () =
  let module C = Circuit.With_interface (Top.I) (Top.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day04" Top.(hierarchical scope) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let () = generate_day04_rtl ()
