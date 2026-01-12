(* Day 3 Solution *)

open! Core
open! Hardcaml
open! Signal

let num_bits = 32

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; ascii_char : 'a With_valid.t [@bits 8]
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { total_joltage : 'a [@bits num_bits]
    ; done_ : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Calculate
    | Add
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; reset_n; ascii_char } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let done_ = Variable.wire ~default:gnd () in
  let%hw_var prev_char = Variable.reg spec ~width:8 in
  let%hw_var total_joltage = Variable.reg spec ~width:num_bits in
  let%hw_var tens_place = Variable.reg spec ~width:num_bits in
  let%hw_var ones_place = Variable.reg spec ~width:num_bits in
  let of_ascii c = uresize ~width:num_bits (c -:. 0x30) in
  compile
    [ when_
        ascii_char.valid
        [ prev_char <-- ascii_char.value
        ; sm.switch
            [ Idle, [ sm.set_next Calculate ]
            ; ( Calculate
              , [ if_
                    (ascii_char.value ==: of_char '\n')
                    [ when_
                        (of_ascii prev_char.value >: ones_place.value)
                        [ ones_place <-- of_ascii prev_char.value ]
                    ; sm.set_next Add
                    ]
                    [ if_
                        (of_ascii prev_char.value >: tens_place.value)
                        [ tens_place <-- of_ascii prev_char.value; ones_place <--. 0 ]
                        [ when_
                            (of_ascii prev_char.value >: ones_place.value)
                            [ ones_place <-- of_ascii prev_char.value ]
                        ]
                    ]
                ] )
            ; ( Add
              , [ total_joltage
                  <-- total_joltage.value
                      +: uresize
                           ~width:num_bits
                           (of_unsigned_int ~width:8 10 *: tens_place.value)
                      +: ones_place.value
                ; tens_place <--. 0
                ; ones_place <--. 0
                ; if_
                    (* TODO: case on EOT instead of 'x' *)
                    (ascii_char.value ==: of_char 'x')
                    [ sm.set_next Done ]
                    [ sm.set_next Calculate ]
                ] )
            ; Done, [ done_ <-- vdd ]
            ]
        ]
    ];
  { total_joltage = total_joltage.value; done_ = done_.value }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"solution" create
;;
