(* Top-level module for Day 4 *)

open! Core
open! Hardcaml
open! Signal
open Aof_infra

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; uart_rx : 'a
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

let create scope ({ clock; reset_n; uart_rx } : _ I.t) : _ O.t =
  let receiver = Uart_rx.hierarchical scope { clock; reset_n; rx = uart_rx } in
  let solution = Solution.create scope { clock; reset_n; ascii_char = receiver.rx_byte } in
  let num_to_display = solution.num_rolls in
  let display =
    Seven_segment_display.hierarchical scope { clock; reset_n; num = num_to_display }
  in
  { anode_n = display.anode_n; seg = display.seg; dp = display.dp }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"day04_top" create
;;
