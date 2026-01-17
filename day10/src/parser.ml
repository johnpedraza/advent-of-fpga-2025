(* Input parser for Day 10: Factory *)

open! Core
open! Hardcaml
open! Signal

let max_light_width = 10
let max_num_buttons = 13
let max_num_joltages = 13
let counter_bits = 8
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
    { desired_lights : 'a [@bits max_light_width]
    ; buttons : 'a array [@length max_num_buttons] [@bits max_light_width]
    ; joltages : 'a array [@length max_num_joltages] [@bits 32]
    ; valid : 'a
    ; all_input_received : 'a
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Lights
    | Buttons
    | Joltages
    | Valid
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; reset_n; ascii_char } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let valid = Variable.wire ~default:gnd () in
  let desired_lights = Variable.reg spec ~width:max_light_width in
  let clear_buttons = Variable.wire ~default:gnd () in
  (* Keep track of the available buttons for the current machine *)
  let buttons =
    Array.init max_num_buttons ~f:(fun _ ->
      Variable.reg spec ~clear:clear_buttons.value ~width:max_light_width)
  in
  (* Keep track of which indicator light to parse next *)
  let%hw_var light_counter = Variable.reg spec ~width:counter_bits in
  (* Keep track of which button to parse next *)
  let%hw_var button_counter = Variable.reg spec ~width:counter_bits in
  (* Based on current button counter value, set a value to it *)
  let set_button ~light_index =
    let mask =
      uresize
        ~width:max_light_width
        (binary_to_onehot
           (of_unsigned_int ~width:counter_bits (max_light_width - 1) -: light_index))
    in
    Array.to_list
      (Array.mapi buttons ~f:(fun i b ->
         Always.when_ (button_counter.value ==:. i) [ b <-- (b.value |: mask) ]))
  in
  (* Assert when End of Transmission signal is received *)
  let%hw_var all_input_received = Variable.reg spec ~width:1 in
  compile
    [ when_
        ascii_char.valid
        [ when_ (ascii_char.value ==:. eot ) [ all_input_received <--. 1 ]
        ; sm.switch
            [ ( Idle
              , [ sm.set_next Lights
                ; light_counter <--. 0
                ; desired_lights <--. 0
                ; button_counter <--. 0
                ; clear_buttons <-- vdd
                ] )
            ; ( Lights
              , [ if_
                    (ascii_char.value ==: of_char ']')
                    [ sm.set_next Buttons ]
                    (* OR the 10-bit lights register with one-hot mask *)
                    [ when_
                        (ascii_char.value ==: of_char '#')
                        [ desired_lights
                          <-- (desired_lights.value
                               |: uresize
                                    ~width:max_light_width
                                    (binary_to_onehot
                                       (of_unsigned_int
                                          ~width:counter_bits
                                          (max_light_width - 1)
                                        -: light_counter.value)))
                        ]
                    ; light_counter <-- light_counter.value +:. 1
                    ]
                ] )
            ; ( Buttons
              , [ if_
                    (ascii_char.value ==: of_char '{')
                    [ sm.set_next Joltages ]
                    [ if_
                        (ascii_char.value ==: of_char ')')
                        [ button_counter <-- button_counter.value +:. 1 ]
                        [ when_
                            (ascii_char.value
                             >=: of_char '0'
                             &: (ascii_char.value <=: of_char '9'))
                            (set_button ~light_index:(ascii_char.value -:. 0x30))
                        ]
                    ]
                ] )
            ; ( Joltages
              , [ if_ (ascii_char.value ==: of_char '}') [ sm.set_next Valid ] [] ] )
            ; Valid, [ valid <--. 1; sm.set_next Idle ]
            ]
        ]
    ];
  { desired_lights = desired_lights.value
  ; buttons = Array.map buttons ~f:(fun b -> b.value)
  ; joltages = Array.init max_num_joltages ~f:(fun _ -> zero 32)
  ; valid = valid.value
  ; all_input_received = all_input_received.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"parser" create
;;
