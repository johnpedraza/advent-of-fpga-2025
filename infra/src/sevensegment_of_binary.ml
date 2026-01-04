(*
   * Given a hex digit, produce the 7-Segment Display signals
*)
open! Core
open! Hardcaml
open! Signal

let sevensegment_of_binary num =
  cases
    ~default:(of_string "1111111")
    num
    [ of_unsigned_int ~width:4 0, of_string "1000000"
    ; of_unsigned_int ~width:4 1, of_string "1111001"
    ; of_unsigned_int ~width:4 2, of_string "0100100"
    ; of_unsigned_int ~width:4 3, of_string "0110000"
    ; of_unsigned_int ~width:4 4, of_string "0011001"
    ; of_unsigned_int ~width:4 5, of_string "0010010"
    ; of_unsigned_int ~width:4 6, of_string "0000010"
    ; of_unsigned_int ~width:4 7, of_string "1111000"
    ; of_unsigned_int ~width:4 8, of_string "0000000"
    ; of_unsigned_int ~width:4 9, of_string "0010000"
    ; of_unsigned_int ~width:4 10, of_string "0001000"
    ; of_unsigned_int ~width:4 11, of_string "0000011"
    ; of_unsigned_int ~width:4 12, of_string "1000110"
    ; of_unsigned_int ~width:4 13, of_string "0100001"
    ; of_unsigned_int ~width:4 14, of_string "0000110"
    ; of_unsigned_int ~width:4 15, of_string "0001110"
    ]
;;
