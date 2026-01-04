(*
   * Read data bytes of UART transmission from desktop computer and display
 * running sum on 7-Segment display
 *
 * This module exists to test that my infra is working.
*)
open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; uart_rx : 'a
    }
  [@@deriving hardcaml]
end

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
  let clear = ~:reset_n in
  let counter =
    reg_fb
      (Reg_spec.create ~clock ~clear ())
      ~enable:receiver.rx_byte.valid
      ~width:32
      ~f:(fun d -> d +: uresize ~width:32 receiver.rx_byte.value)
  in
  let display =
    Seven_segment_display.hierarchical scope { clock; reset_n; num = counter }
  in
  { anode_n = display.anode_n; seg = display.seg; dp = display.dp }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"add_uart" create
;;
