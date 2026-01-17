(* Day 3 Solution *)

open! Core
open! Hardcaml
open! Signal

let num_bits = 64
let num_batteries = 12 (* Changing this value to 2 will yield solution to part 1 *)
let eot = 0x04 (* End of Transmission *)

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
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; reset_n; ascii_char } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let done_ = Variable.wire ~default:gnd () in
  (* Store all (12) ON battery values contiguously *)
  let%hw_var on_batteries = Variable.reg spec ~width:(num_batteries * 8) in
  let%hw_var total_joltage = Variable.reg spec ~width:num_bits in
  (* Given a vector of batteries and a new value,
   * produce all possibilities in parallel of turning OFF each battery
   * and inserting new battery on the right *)
  let possible_replacements ~batteries ~new_value =
    let replace_battery idx =
      if idx = 0
      then batteries.:[(8 * num_batteries) - 1, 8] @: new_value
      else if idx = num_batteries - 1
      then batteries.:[(8 * num_batteries) - 9, 0] @: new_value
      else
        batteries.:[(8 * num_batteries) - 1, 8 * (idx + 1)]
        @: batteries.:[(8 * idx) - 1, 0]
        @: new_value
    in
    List.init num_batteries ~f:(fun i -> replace_battery i)
  in
  let greatest_vector ~possibilities =
    let greater_vector v1 v2 = mux2 (v1 >=: v2) v1 v2 in
    tree ~arity:2 possibilities ~f:(reduce ~f:greater_vector)
  in
  let new_greatest ~new_value =
    greatest_vector
      ~possibilities:(possible_replacements ~batteries:on_batteries.value ~new_value)
  in
  let rec int_pow base n =
    match n with
    | 0 -> 1
    | 1 -> base
    | _ ->
      let b = int_pow base (n / 2) in
      b * b * if n mod 2 = 0 then 1 else base
  in
  (* Given a battery bank, calculate joltage of the line *)
  let battery_bank_joltage ~batteries =
    let battery_list = split_lsb ~part_width:8 ~exact:true batteries in
    let rec total blist i =
      match blist with
      | [] -> zero num_bits
      | hd :: tl ->
        uresize ~width:num_bits (of_unsigned_int ~width:num_bits (int_pow 10 i) *: hd)
        +: total tl (i + 1)
    in
    total battery_list 0
  in
  (* Given N batteries currently switched ON and a new battery value, produce
   * a new set of N batteries by switching OFF an existing battery and
   * inserting the new value on the right. Or leave the battery bank as-is if
   * the new value does not provide a better option *)
  let max_possibility = new_greatest ~new_value:(ascii_char.value -:. 0x30) in
  compile
    [ when_
        ascii_char.valid
        [ sm.switch
            [ ( Idle
              , [ when_
                    (max_possibility >: on_batteries.value)
                    [ on_batteries <-- max_possibility ]
                ; sm.set_next Calculate
                ] )
            ; ( Calculate
              , [ if_
                    (ascii_char.value ==: of_char '\n')
                    [ total_joltage
                      <-- total_joltage.value
                          +: battery_bank_joltage ~batteries:on_batteries.value
                    ; on_batteries <--. 0 (* reset battery bank for next line *)
                    ]
                    [ if_
                        (ascii_char.value ==:. eot)
                        [ sm.set_next Done ]
                        [ when_
                            (max_possibility >: on_batteries.value)
                            [ on_batteries <-- max_possibility ]
                        ]
                    ]
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
