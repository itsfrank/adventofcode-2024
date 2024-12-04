open Aoc_common
(* open Containers *)

(* state machine *)
module Stm = struct
  type state =
    | M_or_D (* initial state, looks for M or D *)
    | D (* used when we're disabled *)
    | U (* u *)
    | L (* l *)
    | Po_num (* paren open '(' *)
    (* number, with tuple: (digit, value) *)
    | Num1 of int
    | Num2 of int
    (* do/dont *)
    | O
    | N_or_Po
    | Ap
    | T
    | Po_dont
    | Pc_do
    | Pc_dont

  type result =
    | Shift of state
    | Reset of state
    (* next state, num *)
    | Emit of (state * int)
    | Complete of (state * int)
    | Enable of state
    | Disable of state

  let init = M_or_D

  let init_from c =
    match c with
    | 'm' -> U
    | 'd' -> O
    | _ -> M_or_D
  ;;

  let int_of_chr c = Char.code c - Char.code '0'

  let consume c sm =
    match sm with
    | M_or_D ->
      (match c with
       | 'm' -> Shift U
       | 'd' -> Shift O
       | _ -> Reset (init_from c))
    | D -> if c = 'd' then Shift O else Reset D
    | U -> if c = 'u' then Shift L else Reset (init_from c)
    | L -> if c = 'l' then Shift Po_num else Reset (init_from c)
    | Po_num -> if c = '(' then Shift (Num1 0) else Reset (init_from c)
    | Num1 value ->
      (match c with
       | '0' .. '9' -> Shift (Num1 ((value * 10) + int_of_chr c))
       | ',' -> Emit (Num2 0, value)
       | _ -> Reset (init_from c))
    | Num2 value ->
      (match c with
       | '0' .. '9' -> Shift (Num2 ((value * 10) + int_of_chr c))
       | ')' -> Complete (M_or_D, value)
       | _ -> Reset (init_from c))
    | O -> if c = 'o' then Shift N_or_Po else Reset (init_from c)
    | N_or_Po ->
      (match c with
       | 'n' -> Shift Ap
       | '(' -> Shift Pc_do
       | _ -> Reset (init_from c))
    | Ap -> if c = '\'' then Shift T else Reset (init_from c)
    | T -> if c = 't' then Shift Po_dont else Reset (init_from c)
    | Po_dont -> if c = '(' then Shift Pc_dont else Reset (init_from c)
    | Pc_do -> if c = ')' then Enable M_or_D else Reset (init_from c)
    | Pc_dont -> if c = ')' then Disable M_or_D else Reset (init_from c)
  ;;
end

let str_pop_front s =
  if String.length s = 0
  then None
  else Some (String.get s 0, String.sub s 1 (String.length s - 1))
;;

let donts = ref 0
let dos = ref 0

let process_line ~check_do l =
  let rec helper acc num st l =
    match str_pop_front l with
    | None -> acc
    | Some (c, l) ->
      (match Stm.consume c st with
       | Reset st -> helper acc 0 st l
       | Shift st -> helper acc num st l
       | Emit (st, v) -> helper acc v st l
       | Complete (st, v) -> helper (acc + (v * num)) 0 st l
       | Enable st ->
         dos := !dos + 1;
         helper acc 0 st l
       | Disable st ->
         donts := !donts + 1;
         let st = if check_do then Stm.D else st in
         helper acc 0 st l)
  in
  helper 0 0 Stm.init l
;;

let parse_input file_path = Common.read_lines file_path |> String.concat ""
let do_part1 input = input |> process_line ~check_do:false
let do_part2 input = input |> process_line ~check_do:true

module Test = struct
  open Alcotest

  let%test "s1 sample" =
    let res = Common.resource_file "day_3/sample.txt" |> parse_input |> do_part1 in
    (check int) "s1 sample" 161 res
  ;;

  let%test "s1 puzzle" =
    let res = Common.resource_file "day_3/puzzle.txt" |> parse_input |> do_part1 in
    (check int) "s1 puzzle" 184576302 res
  ;;

  let%test "s2 sample" =
    let res = Common.resource_file "day_3/sample.txt" |> parse_input |> do_part2 in
    (check int) "s2 sample" 48 res
  ;;

  let%test "s2 puzzle" =
    dos := 0;
    donts := 0;
    let res = Common.resource_file "day_3/puzzle.txt" |> parse_input |> do_part2 in
    Printf.printf "dos: %d\n" !dos;
    Printf.printf "donts: %d\n" !donts;
    (check int) "s2 puzzle" 118173507 res
  ;;
end
