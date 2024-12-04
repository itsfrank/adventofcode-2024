open Aoc_common
open Containers

(* converts input into two lists *)
let parse_input filename =
  filename
  |> Common.as_tokens
  |> List.fold_left
       (fun acc t ->
         match t with
         | Common.Newline ->
           (match acc with
            | [] -> []
            | hd :: tl -> [] :: List.rev hd :: tl)
         | Common.Word w ->
           (match Int.of_string w with
            | None -> failwith ""
            | Some v ->
              (match acc with
               | [] -> [ [ v ] ]
               | hd :: tl -> (v :: hd) :: tl)))
       []
  |> function
  | [] -> []
  | hd :: tl -> List.rev hd :: tl |> List.rev
;;

let pairwise_diff l =
  let rec helper acc l =
    match l with
    | [] | [ _ ] -> acc
    | a :: b :: tl -> helper ((a - b) :: acc) (b :: tl)
  in
  List.rev (helper [] l)
;;

let is_safe l =
  let diff = pairwise_diff l in
  let sign = List.hd diff in
  let rec helper s l =
    match l with
    | [] -> true
    | v :: tl -> if v = 0 || abs v > 3 || v * s <= 0 then false else helper s tl
  in
  helper sign diff
;;

(* keeping the evidence of my attempt at a fancy solution *)
(* I'm sure i could make it work, but we have to move on :( *)
let is_safe2 ?(d = false) l =
  let diff = pairwise_diff l in
  let rec helper ?(d = false) l =
    match l with
    | [] -> true
    | [ v ] ->
      if v = 0 || abs v > 3 then d else true
    | [ v; b ] ->
      if v = 0 || abs v > 3 || v * b < 0
      then if d then helper [ b + v ] || helper [ v ] else false
      else helper ~d [ b ]
    | v :: b :: tl ->
      if v = 0 || abs v > 3 || v * b < 0
      then if d then helper ~d:false ((v + b) :: tl) else false
      else helper ~d (b :: tl)
  in
  (* handle first element being bad *)
  match diff with
  | [] | [ _ ] -> true
  | v :: b :: tl ->
    (if d && (v = 0 || abs v > 3 || v * b < 0)
     then (
       helper (b :: tl))
     else false)
    || helper ~d diff
;;

let is_safe3 l =
  let is_safe_impl l =
    let diff = pairwise_diff l in
    let sign = List.hd diff in
    let rec helper i s l =
      match l with
      | [] -> true, -1
      | v :: tl ->
        if v = 0 || abs v > 3 || v * s <= 0 then false, i else helper (i + 1) s tl
    in
    helper 0 sign diff
  in
  is_safe_impl l
;;

let do_part1 input =
  input
  |> List.fold_left
       (fun acc l ->
         match is_safe2 l with
         | true -> acc + 1
         | false -> acc)
       0
;;

let do_part2 input =
  input
  |> List.fold_left
       (fun acc l ->
         match is_safe3 l with
         | true, _ -> acc + 1
         | false, i ->
           let res1, _ = List.remove_at_idx i l |> is_safe3 in
           let res2, _ = List.remove_at_idx (i + 1) l |> is_safe3 in
           let res3, _ = List.remove_at_idx (i - 1) l |> is_safe3 in
           (match res1 || res2 || res3 with
            | true -> acc + 1
            | false -> acc))
       0
;;

module Test = struct
  open Alcotest

  let%test "parse input" =
    let data = Common.resource_file "day_2/sample.txt" |> parse_input in
    (check (list (list int)))
      "c"
      [ [ 7; 6; 4; 2; 1 ]
      ; [ 1; 2; 7; 8; 9 ]
      ; [ 9; 7; 6; 2; 1 ]
      ; [ 1; 3; 2; 4; 5 ]
      ; [ 8; 6; 4; 4; 1 ]
      ; [ 1; 3; 6; 7; 9 ]
      ]
      data
  ;;

  let%test "pairwise diff" =
    (check (list int)) "c" [ 1; 2; 2; 1 ] (pairwise_diff [ 7; 6; 4; 2; 1 ])
  ;;

  let%test "s1 sample" =
    let res = Common.resource_file "day_2/sample.txt" |> parse_input |> do_part1 in
    (check int) "c" 2 res
  ;;

  let%test "s1 main" =
    let res = Common.resource_file "day_2/puzzle.txt" |> parse_input |> do_part1 in
    (check int) "c" 242 res
  ;;

  let%test "s2 alternate cases" =
    [ "first diff is 0", [ 1; 1; 2; 3; 4 ]
    ; "0 in middle", [ 0; 1; 1; 2; 3; 4 ]
    ; "0 at end", [ 1; 2; 3; 4; 4 ]
    ; "too big at start", [ -10; 2; 3; 4 ]
    ; "too big at middle", [ 1; -10; 2; 3; 4 ]
    ; "too big at near end", [ 1; 2; 3; -10; 4 ]
    ; "too big at  end", [ 1; 2; 3; 4; 40 ]
    ; "sign change start", [ 3; 2; 3; 4; 5 ]
    ; "sign change start different", [ 3; 2; 4; 5; 6 ]
    ; "sign change end", [ 1; 2; 3; 5; 4 ]
    ; "sign change end different", [ 1; 2; 3; 5; 3 ]
    ; "sign change middle", [ 1; 3; 2; 4; 5 ]
    ]
    |> List.iter (fun (name, l) ->
      let res = do_part2 [ l ] in
      (check int) name 1 res)
  ;;

  let%test "s2 sample" =
    let res = Common.resource_file "day_2/sample.txt" |> parse_input |> do_part2 in
    (check int) "sum" 4 res
  ;;

  let%test "s2 main" =
    let res = Common.resource_file "day_2/puzzle.txt" |> parse_input |> do_part2 in
    (check int) "sum" 311 res
  ;;
end
