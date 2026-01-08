(* Datapath for Day 1 solution(s) *)

open! Core
open! Hardcaml
open! Signal

module I = struct
  type 'a t =
    { clock : 'a
    ; reset_n : 'a
    ; puzzle_char : 'a With_valid.t [@bits 8]
    ; direction : 'a
    ; is_digit : 'a
    ; is_newline : 'a
    }
  [@@deriving hardcaml]
end

module O = struct
  type 'a t =
    { zero_count_part1 : 'a [@bits 32]
    ; zero_count_part2 : 'a [@bits 32]
    }
  [@@deriving hardcaml]
end

let create ({ clock; reset_n; puzzle_char; direction; is_digit; is_newline } : _ I.t)
  : _ O.t
  =
  let spec = Reg_spec.create ~clock ~reset:reset_n ~reset_edge:Edge.Falling () in
  (* Convert ascii digit to numerical value, e.g. '5' = 0x35 -> 5 *)
  let digit_num = puzzle_char.value -: of_string "8'h30" in
  (* Registers to keep track of most recent 2 digits encountered.
   * This implicitly handles the modulo 100 nature of the dial *)
  let ones_place =
    reg spec ~clear:is_newline ~enable:(is_digit &: puzzle_char.valid) digit_num
  in
  let tens_place = reg spec ~clear:is_newline ~enable:puzzle_char.valid ones_place in
  (* Current rotation value *)
  let rotation_mod_100 =
    uresize ~width:8 (tens_place *: of_unsigned_int ~width:8 10) +: ones_place
  in
  (* Count up number of full rotations as rotation amount is parsed.
   * This is essentially equivalent to num_rotations / 100 *)
  let full_rotations =
    reg_fb
      spec
      ~clear:is_newline
      ~enable:(is_digit &: puzzle_char.valid)
      ~width:32
      ~f:(fun q ->
        uresize ~width:32 (q *: of_unsigned_int ~width:32 10)
        +: uresize ~width:32 tens_place)
  in
  (* Given position, rotation, and direction, calculate new dial position *)
  let turn_dial position rotation direction =
    mux2
      (direction ==:. 0) (* left *)
      (mux2
         (position <: rotation)
         (of_string "8'd100" -: (rotation -: position))
         (position -: rotation))
      (mux2
         (position +: rotation >: of_string "8'd99")
         (position +: rotation -: of_string "8'd100")
         (position +: rotation))
  in
  let dial_position =
    reg_fb
      spec
      ~reset_to:(Bits.of_unsigned_int ~width:8 50)
      ~enable:(is_newline &: puzzle_char.valid)
      ~width:8
      ~f:(fun q -> turn_dial q rotation_mod_100 direction)
  in
  let result_pos = turn_dial dial_position rotation_mod_100 direction in
  (* Calculate number of zeros to add to accumulator based on information
     from current puzzle character *)
  let new_zeros_part1 = mux2 (is_newline ==: vdd) (result_pos ==:. 0) gnd in
  let new_zeros_part2 =
    let landed_on_zero = result_pos ==:. 0 in
    let dial_nonzero = dial_position <>:. 0 in
    let dial_passing_zero =
      let left_turn = direction ==:. 0 &: (result_pos >: dial_position) in
      let right_turn = direction ==:. 1 &: (result_pos <: dial_position) in
      dial_nonzero &: (left_turn |: right_turn)
    in
    let plus_one = landed_on_zero |: dial_passing_zero in
    mux2 is_newline (full_rotations +: uresize ~width:32 plus_one) (zero 32)
  in
  (* Update overall zero counts *)
  let zero_count_part1 =
    reg_fb spec ~enable:puzzle_char.valid ~width:32 ~f:(fun q ->
      q +: uresize ~width:32 new_zeros_part1)
  in
  let zero_count_part2 =
    reg_fb spec ~enable:puzzle_char.valid ~width:32 ~f:(fun q -> q +: new_zeros_part2)
  in
  { zero_count_part1; zero_count_part2 }
;;
