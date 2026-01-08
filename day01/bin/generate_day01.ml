(* 
 * Generate RTL (Verilog) for Day 1 solution.
 * Includes infrastructure like UART receiver and 7-segment display logic.
 *)

open! Core
open! Hardcaml
open Day01

let generate_day01_rtl () =
  let module C = Circuit.With_interface (Top.I) (Top.O) in
  let scope = Scope.create ~auto_label_hierarchical_ports:true () in
  let circuit = C.create_exn ~name:"day01_top" (Top.(hierarchical scope)) in
  let rtl_circuits =
    Rtl.create ~database:(Scope.circuit_database scope) Verilog [ circuit ]
  in
  let rtl = Rtl.full_hierarchy rtl_circuits |> Rope.to_string in
  print_endline rtl
;;

let () = generate_day01_rtl ()
;;
