(* Controller for Day 1 solution.
 *
 * Read each ASCII char and determine:
 * - current direction
 * - is char a digit?
 * - is char a newline?
 * - is transmission done?
 *
*)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; ascii : 'a With_valid.t [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { direction : 'a
    ; is_digit : 'a
    ; is_newline : 'a
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

let create ({ clock; reset_n; ascii } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let is_L = ascii.value ==: of_string "8'h4C" in
  let is_R = ascii.value ==: of_string "8'h52" in
  let is_dir = is_L |: is_R in
  (* Keep track of most recent direction. L = 0, R = 1 *)
  let direction = reg spec ~enable:(is_dir &: ascii.valid) is_R in
  let is_digit =
    of_string "8'h29" <: ascii.value &: (ascii.value <: of_string "8'h3A") &: ascii.valid
  in
  let is_newline = ascii.value ==: of_string "8'h0A" &: ascii.valid in
  (* I'm not doing anything with the done signal right now... maybe I'll 
     do something with it later *)
  let done_ = ascii.value ==: of_string "8'h04" in
  { direction; is_digit; is_newline; done_ }
;;
