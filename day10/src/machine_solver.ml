(* Given a parsed machine line, return the fewest button presses required
 * to correctly configure the indicator lights *)

open! Core
open! Hardcaml
open! Signal

let max_light_width = 10
let max_num_buttons = 13
let max_num_joltages = 13
let num_bits = 32

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; desired_lights : 'a [@bits max_light_width]
    ; buttons_in : 'a array [@length max_num_buttons] [@bits max_light_width]
    ; _joltages : 'a array [@length max_num_joltages] [@bits 32]
    ; start : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { fewest_button_presses : 'a With_valid.t [@bits num_bits] }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Calculate
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create
  scope
  ({ clock; reset_n; desired_lights; buttons_in; _joltages; start } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let%hw_var valid = Variable.wire ~default:gnd () in
  (* Store the lights you want in a local register *)
  let%hw_var goal_lights = Variable.reg spec ~width:max_light_width in
  (* Array of registers, one for each button *)
  let buttons =
    Array.init max_num_buttons ~f:(fun _ -> Variable.reg spec ~width:max_light_width)
  in
  (* Load the buttons from the input into local registers *)
  let load_buttons =
    Array.to_list (Array.mapi buttons ~f:(fun i b -> b <-- buttons_in.(i)))
  in
  (* Accumulator: the running result of button pressing permutations *)
  let%hw_var accumulator = Variable.reg spec ~width:max_light_width in
  (* Counter to be incremented in binary and interpreted as gray code *)
  let%hw_var counter = Variable.reg spec ~width:max_num_buttons in
  (* Minimum number of button presses to achieve desired lights *)
  let%hw_var min_presses = Variable.reg spec ~width:8 in
  (* Which button to try pressing next based on sequential gray codes *)
  let%hw_var toggle_index = Variable.wire ~default:(zero 8) () in
  (* Given the current counter, use gray code trickery to determine which button
   * to press next *)
  let calculate_toggle_index =
    let onehot = binary_to_gray (counter.value +:. 1) ^: binary_to_gray counter.value in
    uresize ~width:8 (onehot_to_binary onehot)
  in
  (* Based on which bit was toggled between adjacent gray codes, update accumulator *)
  let update_accumulator =
    Array.to_list
      (Array.mapi buttons ~f:(fun i b ->
         when_
           (toggle_index.value ==:. i)
           [ accumulator <-- accumulator.value ^: b.value ]))
  in
  compile
    [ sm.switch
        [ ( Idle
          , [ when_
                start
                (load_buttons
                 @ [ sm.set_next Calculate
                   ; min_presses <-- ones 8
                   ; counter <--. 0
                   ; goal_lights <-- desired_lights
                   ])
            ] )
        ; ( Calculate
          , [ when_ (counter.value >=: ones max_num_buttons) [ sm.set_next Done ]
            ; when_
                (accumulator.value ==: goal_lights.value)
                [ when_
                    (uresize ~width:8 (popcount (binary_to_gray counter.value))
                     <: min_presses.value)
                    [ min_presses
                      <-- uresize ~width:8 (popcount (binary_to_gray counter.value))
                    ]
                ]
            ; counter <-- counter.value +:. 1
            ; toggle_index <-- calculate_toggle_index
            ]
            @ update_accumulator )
        ; Done, [ valid <-- vdd ]
        ]
    ];
  { fewest_button_presses =
      { value = uresize ~width:32 min_presses.value; valid = valid.value }
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"machine_solver" create
;;
