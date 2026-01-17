(* Day 4 Solution *)

open! Core
open! Hardcaml
open! Signal

let num_bits = 32
let word_width = 8
let bram_size = 65536
let bram_addr_width = 16
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
    { num_rolls : 'a [@bits num_bits]
    ; done_ : 'a
    ; perf_counter : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

module States = struct
  type t =
    | Idle
    | Pack_input
    | Load_input
    | Reset_buffer
    | Read_top_row
    | Read_mid_row
    | Read_bot_row
    | Mem_delay
    | Calculate_accessible_rolls
    | Done
  [@@deriving sexp_of, compare ~localize, enumerate]
end

let packing_masks =
  List.init word_width ~f:(fun i ->
    let s = String.init word_width ~f:(fun j -> if j = i then '1' else '0') in
    of_string s)
;;

(* Generate word_width adders in parallel to sum the adjacent roll counts
 * for word_width positions at once *)
let adjacent_adders ~curr_top ~curr_mid ~curr_bot =
  let left = word_width + 1 in
  let center = word_width in
  let right = word_width - 1 in
  List.init word_width ~f:(fun i ->
    tree ~arity:2 ~f:(reduce ~f:( +: ))
    [uresize ~width:num_bits curr_top.:[left - i, left - i]
    ; uresize ~width:num_bits curr_top.:[center - i, center - i]
    ; uresize ~width:num_bits curr_top.:[right - i, right - i]
    ; uresize ~width:num_bits curr_mid.:[left - i, left - i]
    ; uresize ~width:num_bits curr_mid.:[right - i, right - i]
    ; uresize ~width:num_bits curr_bot.:[left - i, left - i]
    ; uresize ~width:num_bits curr_bot.:[center - i, center - i]
    ; uresize ~width:num_bits curr_bot.:[right - i, right - i]])
;;

(* Given the word_width sums and the middle row containing whether or not that
 * position is a roll, return the sum of rolls that can be moved within
 * that word_width number of positions on the shelf *)
let sum_of_movable_rolls ~curr_top ~curr_mid ~curr_bot =
  let adders = adjacent_adders ~curr_top ~curr_mid ~curr_bot in
  let row_list = split_msb ~part_width:1 ~exact:true curr_mid.:[word_width, 1] in
  (* spots where adjacent number < 4 *)
  let movable_spots = List.map ~f:(fun sum -> sum <:. 4) adders in
  let movable_roll_list =
    List.map2_exn ~f:(fun a b -> uresize ~width:num_bits (a &: b)) row_list movable_spots
  in
  tree ~arity:2 ~f:(reduce ~f:( +: )) movable_roll_list
;;

let create scope ({ clock; reset_n; ascii_char } : _ I.t) : _ O.t =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  let open Always in
  let sm = State_machine.create (module States) spec in
  let done_ = Variable.wire ~default:gnd () in
  let prev_char = Variable.reg spec ~width:8 in
  (* Total number of rolls that can be safely removed *)
  let num_rolls = Variable.reg spec ~width:num_bits in
  (* Keep track of puzzle input dimensions *)
  let%hw_var num_rows = Variable.reg spec ~width:bram_addr_width in
  (* Initialize Block RAM to hold puzzle input *)
  let%hw_var input_bram_addr = Variable.wire ~default:(zero bram_addr_width) () in
  let%hw_var input_bram_addr_reg = Variable.reg spec ~width:bram_addr_width in
  let%hw_var input_bram_we = Variable.wire ~default:gnd () in
  let%hw_var input_bram_re = Variable.wire ~default:gnd () in
  let%hw_var input_bram_wdata = Variable.wire ~default:(zero word_width) () in
  let write_port =
    { Write_port.write_clock = clock
    ; write_address = input_bram_addr.value
    ; write_enable = input_bram_we.value
    ; write_data = input_bram_wdata.value
    }
  in
  let read_port =
    { Read_port.read_clock = clock
    ; read_address = input_bram_addr.value
    ; read_enable = input_bram_re.value
    }
  in
  (* Single port BRAM *)
  let puzzle_input =
    Ram.create
      ~collision_mode:Read_before_write
      ~size:bram_size
      ~write_ports:[| write_port |]
      ~read_ports:[| read_port |]
      ()
  in
  (* Input buffer for packing bits *)
  let%hw_var input_buffer = Variable.reg spec ~width:word_width in
  let%hw_var input_buffer_count = Variable.reg spec ~width:word_width in
  (* Number of BRAM words per row on the puzzle shelf *)
  let%hw_var words_per_row = Variable.reg spec ~width:bram_addr_width in
  let%hw_var words_per_row_determined = Variable.reg spec ~width:1 in
  (* Current row and col while traversing BRAM 
   * col is based on BRAM structure, not the col on the shelf *)
  let%hw_var row = Variable.reg spec ~width:bram_addr_width in
  let%hw_var col_word = Variable.reg spec ~width:bram_addr_width in
  (* Store currently loaded row sections *)
  let%hw_var curr_top = Variable.reg spec ~width:(word_width + 2) in
  let%hw_var curr_mid = Variable.reg spec ~width:(word_width + 2) in
  let%hw_var curr_bot = Variable.reg spec ~width:(word_width + 2) in
  (* Last 2 columns of each previous row. 
   * Needed to account for shelf positions near BRAM word borders *)
  let%hw_var prev_top = Variable.reg spec ~width:2 in
  let%hw_var prev_mid = Variable.reg spec ~width:2 in
  let%hw_var prev_bot = Variable.reg spec ~width:2 in
  (* Performance counter *)
  let%hw_var perf_counter = Variable.reg spec ~width:32 in
  let%hw_var all_input_received = Variable.reg spec ~width:1 in
  compile
    [ when_
        (~:(done_.value) &: all_input_received.value)
        [ perf_counter <-- perf_counter.value +:. 1 ]
    ; sm.switch
        [ ( Idle
          , [ when_
                ascii_char.valid
                [ sm.set_next Pack_input
                ; input_buffer_count <--. 1
                ; if_
                    (ascii_char.value ==: of_char '@')
                    [ input_buffer <-- of_string ("1" ^ String.make (word_width - 1) '0')
                    ]
                    [ input_buffer <--. 0 ]
                ]
            ] )
        ; ( Pack_input
          , [ when_
                ascii_char.valid
                [ prev_char <-- ascii_char.value
                ; if_
                    (ascii_char.value ==:. eot)
                    [ sm.set_next Read_top_row (* Begin processing *)
                    ; all_input_received <-- vdd
                    ]
                    [ if_
                        (ascii_char.value ==: of_char '\n')
                        [ sm.set_next Load_input; num_rows <-- num_rows.value +:. 1 ]
                        [ input_buffer
                          <-- (input_buffer.value
                               |: mux2
                                    (ascii_char.value ==: of_char '@')
                                    (mux input_buffer_count.value packing_masks)
                                    (zero word_width))
                        ; input_buffer_count <-- input_buffer_count.value +:. 1
                        ; when_
                            (input_buffer_count.value ==:. word_width - 1)
                            [ sm.set_next Load_input ]
                        ]
                    ]
                ]
            ] )
        ; ( Load_input
          , [ when_
                ~:(words_per_row_determined.value)
                [ if_
                    (prev_char.value ==: of_char '\n')
                    [ words_per_row_determined <-- vdd
                    ; words_per_row <-- words_per_row.value +:. 2
                    ]
                    [ words_per_row <-- words_per_row.value +:. 1 ]
                ]
            ; input_bram_we <-- vdd
            ; input_bram_addr <-- input_bram_addr_reg.value
            ; input_bram_addr_reg <-- input_bram_addr_reg.value +:. 1
            ; input_bram_wdata <-- input_buffer.value
            ; if_
                (prev_char.value ==: of_char '\n')
                [ prev_char <-- of_char 'e' (* just so this isn't true again *)
                ; sm.set_next Load_input
                ; input_buffer <-- zero word_width
                ]
                [ sm.set_next Reset_buffer ]
            ] )
        ; ( Reset_buffer
          , [ input_buffer <-- zero word_width
            ; input_buffer_count <-- zero word_width
            ; sm.set_next Pack_input
            ] )
        ; ( Read_top_row
          , [ input_bram_addr
              <-- uresize ~width:bram_addr_width (words_per_row.value *: (row.value -:. 1))
                  +: col_word.value
            ; input_bram_re <-- vdd
            ; sm.set_next Read_mid_row
            ] )
        ; ( Read_mid_row
          , [ if_
                (row.value >:. 0)
                [ curr_top <-- prev_top.value @: puzzle_input.(0) ]
                [ curr_top <--. 0 ]
            ; input_bram_addr
              <-- uresize ~width:bram_addr_width (words_per_row.value *: row.value)
                  +: col_word.value
            ; input_bram_re <-- vdd
            ; sm.set_next Read_bot_row
            ] )
        ; ( Read_bot_row
          , [ curr_mid <-- prev_mid.value @: puzzle_input.(0)
            ; input_bram_addr
              <-- uresize ~width:bram_addr_width (words_per_row.value *: (row.value +:. 1))
                  +: col_word.value
            ; input_bram_re <-- vdd
            ; sm.set_next Mem_delay
            ] )
        ; ( Mem_delay (* Extra stage to deal with memory delay *)
          , [ if_
                (row.value <: num_rows.value -:. 1)
                [ curr_bot <-- prev_bot.value @: puzzle_input.(0) ]
                [ curr_bot <--. 0 ]
            ; sm.set_next Calculate_accessible_rolls
            ] )
        ; ( Calculate_accessible_rolls
          , [ num_rolls
              <-- num_rolls.value
                  +: uresize
                       ~width:num_bits
                       (sum_of_movable_rolls
                          ~curr_top:curr_top.value
                          ~curr_mid:curr_mid.value
                          ~curr_bot:curr_bot.value)
            ; if_
                (col_word.value ==: words_per_row.value -:. 1)
                [ col_word <--. 0; row <-- row.value +:. 1 ]
                [ col_word <-- col_word.value +:. 1 ]
            ; prev_top <-- curr_top.value.:[1, 0]
            ; prev_mid <-- curr_mid.value.:[1, 0]
            ; prev_bot <-- curr_bot.value.:[1, 0]
            ; if_
                (row.value
                 ==: num_rows.value -:. 1
                 &: (col_word.value ==: words_per_row.value -:. 1))
                [ sm.set_next Done ]
                [ sm.set_next Read_top_row ]
            ] )
        ; Done, [ done_ <-- vdd ]
        ]
    ];
  { num_rolls = num_rolls.value; done_ = done_.value; perf_counter = perf_counter.value }
;;

let hierarchical scope =
  let module Scoped = Hierarchy.In_scope (I) (O) in
  Scoped.hierarchical ~scope ~name:"solution" create
;;
