(* Solution to Day 10: Factory *)

open! Core
open! Hardcaml
open! Signal

let num_bits = 32
let num_solvers = 3 (* Number of machine solvers to instantiate in parallel *)
let counter_width = 8

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
    { fewest_button_presses : 'a [@bits num_bits]
    ; valid : 'a
    ; perf_counter : 'a [@bits 64]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Dispatching
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; reset_n; ascii_char } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  (* Initialize input parser *)
  let { desired_lights; buttons; joltages; valid; all_input_received } : _ Parser.O.t =
    Parser.hierarchical scope { clock; reset_n; ascii_char }
  in
  (* Start signals for each solver *)
  let start_signals =
    Array.init num_solvers ~f:(fun _ -> Variable.wire ~default:gnd ())
  in
  (* Initialize machine line solvers *)
  let solvers =
    Array.init num_solvers ~f:(fun i ->
      Machine_solver.hierarchical
        scope
        { clock
        ; reset_n
        ; desired_lights
        ; buttons_in = buttons
        ; _joltages = joltages
        ; start = start_signals.(i).value
        })
  in
  (* Keep track of the next solver to assign a machine to *)
  let%hw_var next_solver = Variable.reg spec ~width:counter_width in
  let start_next_machine =
    Array.to_list
      (Array.mapi start_signals ~f:(fun i s ->
         when_ (next_solver.value ==:. i) [ s <-- vdd ]))
  in
  let fewest_button_presses_value_list =
    solvers |> Array.to_list |> List.map ~f:(fun s -> s.fewest_button_presses.value)
  in
  let fewest_button_presses_valid_list =
    solvers |> Array.to_list |> List.map ~f:(fun s -> s.fewest_button_presses.valid)
  in
  (* Overall time is dominated by UART transmission, so only measure cycles after
   * transmission has finished. This value should be relatively consistent, even
   * if a faster I/O method is used, because the machine solvers are all
   * independent *)
  let%hw_var perf_counter = Variable.reg spec ~width:64 in
  (* Simple job dispatcher *)
  (* Since we maintain one solver per machine, just send the parser
   * output to the next solver whenever the parsed line is ready *)
  compile
    [ when_ all_input_received [ perf_counter <-- perf_counter.value +:. 1 ]
    ; sm.switch
        [ ( Dispatching
          , [ when_
                valid
                ([ when_ (next_solver.value ==:. num_solvers - 1) [ sm.set_next Done ]
                 ; next_solver <-- next_solver.value +:. 1
                 ]
                 @ start_next_machine)
            ] )
        ; Done, []
        ]
    ];
  { fewest_button_presses =
      tree ~arity:2 fewest_button_presses_value_list ~f:(reduce ~f:( +: ))
  ; valid = tree ~arity:2 fewest_button_presses_valid_list ~f:(reduce ~f:( &: ))
  ; perf_counter = perf_counter.value
  }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"solution" create
;;
