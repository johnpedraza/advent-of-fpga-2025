(* Top-level module for Day 1 *)

open! Core
open! Hardcaml
open! Signal
open Aof_infra

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; uart_rx : 'a
    ; part_select : 'a (* Switch to select which result (part 1 or 2) to display *)
    }
  [@@deriving hardcaml]
end

(* Signals for multiplexed 7-segment display *)
module O = struct
  type 'a t =
    { anode_n : 'a [@bits 8]
    ; seg : 'a [@bits 7]
    ; dp : 'a
    }
  [@@deriving hardcaml]
end

let create scope ({ clock; reset_n; uart_rx; part_select } : _ I.t) : _ O.t =
  let receiver = Uart_rx.hierarchical scope { clock; reset_n; rx = uart_rx } in
  let solution = Solution.create scope { clock; reset_n; ascii = receiver.rx_byte } in
  let num_to_display = mux2 part_select solution.result_part2 solution.result_part1 in
  let display =
    Seven_segment_display.hierarchical scope { clock; reset_n; num = num_to_display }
  in
  { anode_n = display.anode_n; seg = display.seg; dp = display.dp }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day01_top" create
;;
