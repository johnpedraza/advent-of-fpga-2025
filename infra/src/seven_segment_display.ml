open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; num : 'a [@bits 32]
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

(*
   * Given an anode select and a (32-bit) number, select a hex (4-bit) digit
 * to display.
 *
 * anode_n is an active-low, one-hot (one-cold?) select of which hex digit
 * display to light up on my dev board. So first convert to one hot and then
 * binary. Use this binary number as a select into a multiplexer to choose
 * which 4-bit digit of the number to display.
*)
let digit_to_display anode_n num =
  let split_num = split_lsb ~part_width:4 num in
  let digit_select = onehot_to_binary ~:anode_n in
  mux digit_select split_num
;;

let create _scope ({ clock; reset_n; num } : _ I.t) : _ O.t =
  let clear = ~:reset_n in
  let spec = Reg_spec.create ~clock ~clear () in
  (* Counter to generate slow (1ms period) clock from fast (100MHz) clock *)
  let reload_val = of_unsigned_int ~width:32 100_000 in
  let counter =
    reg_fb spec ~width:32 ~clear_to:reload_val ~f:(fun d ->
      mux2 (d ==:. 0) reload_val (d -:. 1))
  in
  (* Pulse for one cycle when counter is 1 *)
  let slow_pulse = reg spec (counter ==:. 1) in
  (* Rotate which digit is being displayed on each 1ms pulse *)
  let anode_hot =
    reg_fb spec ~width:8 ~clear_to:(of_unsigned_int ~width:8 0) ~f:(fun d ->
      mux2
        slow_pulse
        (mux2 (d ==:. 0) (of_string "8'b0000_0001") (concat_msb [ d.:[6, 0]; d.:[7, 7] ]))
        d)
  in
  let anode_n = ~:anode_hot in
  (* Given a number and a selected digit, generate 7-segment display signal *)
  let digit = digit_to_display anode_n num in
  let seg = Sevensegment_of_binary.sevensegment_of_binary digit in
  (* Keep decimal point off for now (active-low) *)
  let dp = vdd in
  { O.anode_n; seg; dp }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"seven_segment_display" create
;;
