open Aoc_common
open Containers
module Heap_int = CCHeap.Make_from_compare (CCInt)
module Hashtbl_int = CCHashtbl.Make (Int)

(* converts input into two lists *)
let parse_input filename =
  filename
  |> Common.read_lines
  |> List.map Common.split_whitespace
  |> List.map (fun l ->
    ( Option.get_exn_or "failed to convert number" (Int.of_string (List.hd l))
    , Option.get_exn_or "failed to convert number" (Int.of_string (List.tl l |> List.hd))
    ))
  |> List.split
;;

let make_heaps l1 l2 = Heap_int.of_list l1, Heap_int.of_list l2

let sum_mins h1 h2 =
  assert (Heap_int.size h1 = Heap_int.size h2);
  let rec heap_sum h1 h2 acc =
    match Heap_int.take h1, Heap_int.take h2 with
    | Some (h1, m1), Some (h2, m2) -> heap_sum h1 h2 (acc + Int.abs (m1 - m2))
    | _ -> acc
  in
  heap_sum h1 h2 0
;;

let count_table l =
  let ht = Hashtbl_int.create 0 in
  List.iter (fun e -> Hashtbl_int.incr ht e) l;
  ht
;;

let find_similarity l1 ct =
  List.fold_left
    (fun acc e ->
      let count = Hashtbl_int.get_or ct e ~default:0 in
      acc + (e * count))
    0
    l1
;;

let do_part1 filepath =
  let l1, l2 = parse_input filepath in
  let h1, h2 = make_heaps l1 l2 in
  sum_mins h1 h2
;;

let do_part2 filepath =
  let l1, l2 = parse_input filepath in
  let l2count = count_table l2 in
  find_similarity l1 l2count
;;

module Test = struct
  open Alcotest

  let%test "parse input" =
    let p1, p2 = Common.resource_file "day_1/sample.txt" |> parse_input in
    (check (list int)) "p1" [ 3; 4; 2; 1; 3; 3 ] p1;
    (check (list int)) "p2" [ 4; 3; 5; 3; 9; 3 ] p2
  ;;

  let%test "s1 sample" =
    let res = do_part1 (Common.resource_file "day_1/sample.txt") in
    (check int) "sum" 11 res
  ;;

  let%test "s1 main" =
    let res = do_part1 (Common.resource_file "day_1/puzzle.txt") in
    (check int) "sum" 2176849 res
  ;;

  let%test "s2 sample" =
    let res = do_part2 (Common.resource_file "day_1/sample.txt") in
    (check int) "sum" 31 res
  ;;

  let%test "s2 sample" =
    let res = do_part2 (Common.resource_file "day_1/puzzle.txt") in
    (check int) "sum" 23384288 res
  ;;
end
