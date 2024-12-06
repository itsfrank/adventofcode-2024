open Aoc_common
(* open Containers *)

let parse_input_impl lines =
  let order_l, version_l =
    lines
    |> Common.split_list (fun l -> l = "")
    |> function
    | a :: b :: _ -> a, b
    | _ -> failwith "input hould have 2 sections!"
  in
  ( order_l
    |> List.map (fun s ->
      match String.split_on_char '|' s with
      | [] | [ _ ] -> failwith [%string "value %{s} did not match xx|yy"]
      | a :: b :: _ -> int_of_string a, int_of_string b)
  , version_l |> List.map (fun l -> String.split_on_char ',' l |> List.map int_of_string)
  )
;;

let parse_input filename =
  filename |> Common.read_lines ~skip_blank_lines:false |> parse_input_impl
;;

let middle l = List.length l / 2 |> List.nth l

let make_index l =
  let tbl = Hashtbl.create (List.length l) in
  l |> List.iteri (fun i n -> Hashtbl.add tbl n i);
  tbl
;;

let make_index_middle l = make_index l, middle l

let check_order ord idx =
  ord
  |> List.fold_left
       (fun acc (n1, n2) ->
         match Hashtbl.find_opt idx n1, Hashtbl.find_opt idx n2 with
         | Some i1, Some i2 -> acc && i1 < i2
         | _ -> acc && true)
       true
;;

let do_part1 (ord_l, ver_l) =
  ver_l
  |> List.map make_index_middle
  |> List.fold_left (fun acc (idx, m) -> if check_order ord_l idx then acc + m else acc) 0
;;

let list_of_idx idx =
  let arr = Array.init (Hashtbl.length idx) (fun _ -> 0) in
  idx |> Hashtbl.iter (fun v i -> arr.(i) <- v);
  Array.to_list arr
;;

let fix_l ord_l idx =
  let changed = ref true in
  while !changed do
    changed := false;
    ord_l
    |> List.iter (fun (n1, n2) ->
      match Hashtbl.find_opt idx n1, Hashtbl.find_opt idx n2 with
      | Some i1, Some i2 ->
        if i1 > i2
        then (
          changed := true;
          Hashtbl.replace idx n1 i2;
          Hashtbl.replace idx n2 i1)
      | _ -> ())
  done;
  idx
;;

let do_part2 (ord_l, ver_l) =
  ver_l
  |> List.map (fun l -> make_index l)
  |> List.filter (fun idx -> not (check_order ord_l idx))
  |> List.map (fix_l ord_l)
  |> List.map list_of_idx
  |> List.fold_left (fun acc l -> acc + middle l) 0
;;

module Test = struct
  open Alcotest

  let%test "parse input" =
    let data =
      [ "75|13"; "53|13"; ""; "75,47,61,53,29"; "97,61,53,29,13" ] |> parse_input_impl
    in
    let exp = [ 75, 13; 53, 13 ], [ [ 75; 47; 61; 53; 29 ]; [ 97; 61; 53; 29; 13 ] ] in
    (check (pair (list (pair int int)) (list (list int)))) "" exp data
  ;;

  let%test "s1 sample" =
    let res = Common.resource_file "day_5/sample.txt" |> parse_input |> do_part1 in
    (check int) "s1 sample" 143 res
  ;;

  let%test "s1 puzzle" =
    let res = Common.resource_file "day_5/puzzle.txt" |> parse_input |> do_part1 in
    (check int) "s1 puzzle" 5374 res
  ;;

  let%test "s2 sample" =
    let res = Common.resource_file "day_5/sample.txt" |> parse_input |> do_part2 in
    (check int) "s2 sample" 123 res
  ;;

  let%test "s2 puzzle" =
    let res = Common.resource_file "day_5/puzzle.txt" |> parse_input |> do_part2 in
    (check int) "s2 puzzle" 4260 res
  ;;
end
