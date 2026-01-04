(* UART Receiver *)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; rx : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t = { rx_byte : 'a With_valid.t [@bits 8] } [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Start
    | Data
    | Stop
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let create scope ({ clock; reset_n; rx } : _ I.t) : _ O.t =
  let clear = ~:reset_n in
  let spec = Reg_spec.create ~clock ~clear () in
  let open Always in
  let sm = State_machine.create (module States) ~enable:vdd spec in
  (* Counter used to ensure that samples occur in the middle of UART bits *)
  let%hw_var sample_counter = Variable.reg spec ~width:32 in
  (* Counter to keep track of current bit in data packet *)
  let%hw_var bit_counter = Variable.reg spec ~width:4 in
  (* Load the input bits into a byte *)
  let rx_byte = Variable.reg spec ~width:8 in
  (* Assert valid for one clock cycle when full data packet byte is constructed *)
  let valid = Variable.wire ~default:gnd () in
  (* timing values *)
  let clock_rate = 100_000_000 in
  let baud_rate = 115_200 in
  let full_period = of_unsigned_int ~width:32 (clock_rate / baud_rate) in
  let half_period = of_unsigned_int ~width:32 (clock_rate / baud_rate / 2) in
  (* Synchronize UART signal *)
  let rx_sync =
    let rx_ff1 = reg spec rx in
    let rx_ff2 = reg spec rx_ff1 in
    rx_ff2
  in
  compile
    [ sm.switch
        [ ( Idle
          , [ if_
                rx_sync
                [ sample_counter <--. 0 ]
                [ if_
                    (sample_counter.value ==: half_period)
                    [ sample_counter <--. 0; sm.set_next Start ]
                    [ sample_counter <-- sample_counter.value +:. 1 ]
                ]
            ] )
        ; ( Start
          , [ if_
                (sample_counter.value ==: full_period)
                [ sample_counter <--. 0
                ; sm.set_next Data
                ; bit_counter <--. 0
                ; rx_byte <-- concat_msb [ rx_sync; (srl rx_byte.value ~by:1).:[6, 0] ]
                ]
                [ sample_counter <-- sample_counter.value +:. 1 ]
            ] )
        ; ( Data
          , [ if_
                (sample_counter.value ==: full_period)
                [ rx_byte <-- concat_msb [ rx_sync; (srl rx_byte.value ~by:1).:[6, 0] ]
                ; sample_counter <--. 0
                ; bit_counter <-- bit_counter.value +:. 1
                ; when_ (bit_counter.value ==:. 6) [ sm.set_next Stop ]
                ]
                [ sample_counter <-- sample_counter.value +:. 1 ]
            ] )
        ; ( Stop
          , [ if_
                (sample_counter.value ==: full_period)
                [ sm.set_next Idle; sample_counter <--. 0; valid <--. 1 ]
                [ sample_counter <-- sample_counter.value +:. 1 ]
            ] )
        ]
    ];
  { rx_byte = { value = rx_byte.value; valid = valid.value } }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"uart_rx" create
;;
