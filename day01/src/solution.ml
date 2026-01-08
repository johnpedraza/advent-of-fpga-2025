(* Solution to Day 1 puzzles.
 * Accepts ASCII bytes as input and produces results for parts 1 and 2
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
    { result_part1 : 'a [@bits 32]
    ; result_part2 : 'a [@bits 32]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

let create ({ clock; reset_n; ascii } : _ I.t) : _ O.t =
  let controller = Controller.create { clock; reset_n; ascii } in
  let datapath =
    Datapath.create
      { clock
      ; reset_n
      ; puzzle_char = ascii
      ; direction = controller.direction
      ; is_digit = controller.is_digit
      ; is_newline = controller.is_newline
      }
  in
  { result_part1 = datapath.zero_count_part1
  ; result_part2 = datapath.zero_count_part2
  ; done_ = controller.done_
  }
;;
