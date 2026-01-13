(* Solution to Day 1 *)

open! Core
open! Hardcaml
open! Signal

let num_bits = 32

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
    { result_part1 : 'a [@bits num_bits]
    ; result_part2 : 'a [@bits num_bits]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Direction
    | Turn
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; reset_n; ascii } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let of_ascii c = uresize ~width:num_bits (c -:. 0x30) in
  (* Keep track of direction (L/R) for current dial turn *)
  let%hw_var current_direction = Variable.reg spec ~width:1 in
  (* Keep track of 2 least significant digits of dial turn *)
  let%hw_var tens_place = Variable.reg spec ~width:num_bits in
  let%hw_var ones_place = Variable.reg spec ~width:num_bits in
  (* Current dial position *)
  let%hw_var dial_position =
    Variable.reg spec ~width:num_bits ~reset_to:(Bits.of_unsigned_int ~width:num_bits 50)
  in
  (* Keep track of the number of times the dial makes a full 100 tick cycle
   * during a single turn *)
  let%hw_var full_rotations = Variable.reg spec ~width:num_bits in
  let turn_dial ~position ~rotation ~direction =
    mux2
      (direction ==:. 0) (* left *)
      (mux2
         (position <: rotation)
         (of_string "32'd100" -: (rotation -: position))
         (position -: rotation))
      (mux2
         (position +: rotation >: of_string "32'd99")
         (position +: rotation -: of_string "32'd100")
         (position +: rotation))
  in
  let rotation_mod_100 =
    uresize ~width:num_bits (tens_place.value *: of_unsigned_int ~width:num_bits 10)
    +: ones_place.value
  in
  let result_position =
    turn_dial
      ~position:dial_position.value
      ~rotation:rotation_mod_100
      ~direction:current_direction.value
  in
  (* Determine if the dial passed zero without necessarily making a full 100 tick
   * rotation *)
  let dial_passing_zero =
    let left_turn =
      current_direction.value ==:. 0 &: (result_position >: dial_position.value)
    in
    let right_turn =
      current_direction.value ==:. 1 &: (result_position <: dial_position.value)
    in
    dial_position.value <>:. 0 &: (left_turn |: right_turn)
  in
  (* Result variables *)
  let%hw_var zero_count_part1 = Variable.reg spec ~width:num_bits in
  let%hw_var zero_count_part2 = Variable.reg spec ~width:num_bits in
  let done_ = Variable.wire ~default:gnd () in
  compile
    [ when_
        ascii.valid
        [ sm.switch
            [ ( Direction
              , [ if_
                    (ascii.value ==: of_char 'x')
                    [ sm.set_next Done; done_ <-- vdd ]
                    [ sm.set_next Turn
                    ; if_
                        (ascii.value ==: of_char 'L')
                        [ current_direction <--. 0 ]
                        [ current_direction <--. 1 ]
                    ]
                ; tens_place <--. 0
                ; ones_place <--. 0
                ] )
            ; ( Turn
              , [ if_
                    (ascii.value ==: of_char '\n')
                    [ dial_position <-- result_position
                    ; zero_count_part1
                      <-- zero_count_part1.value
                          +: uresize ~width:num_bits (result_position ==:. 0)
                    ; zero_count_part2
                      <-- zero_count_part2.value
                          +: full_rotations.value
                          +: uresize
                               ~width:32
                               (dial_passing_zero |: (result_position ==:. 0))
                    ; full_rotations <--. 0
                    ; sm.set_next Direction
                    ]
                    [ full_rotations
                      <-- uresize
                            ~width:num_bits
                            (full_rotations.value *: of_unsigned_int ~width:num_bits 10)
                          +: tens_place.value
                    ; ones_place <-- of_ascii ascii.value
                    ; tens_place <-- ones_place.value
                    ]
                ] )
            ; Done, [ done_ <-- vdd ]
            ]
        ]
    ];
  { result_part1 = zero_count_part1.value
  ; result_part2 = zero_count_part2.value
  ; done_ = done_.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"solution" create
;;
