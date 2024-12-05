open Aoc_common
open Containers

(* converts input into two lists *)
let parse_input filename =
  let lines = Common.read_lines filename in
  let grid = Array.init (List.length lines) (fun _ -> [||]) in
  List.iteri
    (fun i s ->
      grid.(i) <- Array.init (String.length s) (fun _ -> '.');
      let row = grid.(i) in
      String.iteri (fun j c -> row.(j) <- c) s)
    lines;
  grid
;;

type state =
  | X
  | M
  | A
  | S

type result =
  | Next of state
  | Fail
  | Find

let check_next c st =
  match st with
  | X ->
    (match c with
     | 'X' -> Next M
     | _ -> Fail)
  | M ->
    (match c with
     | 'M' -> Next A
     | _ -> Fail)
  | A ->
    (match c with
     | 'A' -> Next S
     | _ -> Fail)
  | S ->
    (match c with
     | 'S' -> Find
     | _ -> Fail)
;;

let get_coord (x, y) grid =
  match Array.get_safe grid y with
  | Some arr -> Array.get_safe arr x
  | None -> None
;;

let dirs = [ -1, 0; -1, 1; 0, 1; 1, 1; 1, 0; 1, -1; 0, -1; -1, -1 ]
let add_v2 (x1, y1) (x2, y2) = x1 + x2, y1 + y2

let search coord grid =
  match get_coord coord grid with
  | Some 'X' ->
    dirs
    |> List.fold_left
         (fun acc dir ->
           let rec helper coord st =
             match get_coord coord grid with
             | Some c ->
               (match check_next c st with
                | Next s -> helper (add_v2 coord dir) s
                | Fail -> acc
                | Find -> acc + 1)
             | None -> acc
           in
           helper (add_v2 coord dir) M)
         0
  | _ -> 0
;;

let opposites =
  [ (* (-1, 0), (1, 0); *)
    (* (0, -1), (0, 1); *)
    (1, 1), (-1, -1)
  ; (1, -1), (-1, 1)
  ]
;;

let count_mas coord grid =
  match get_coord coord grid with
  | Some 'A' ->
    opposites
    |> List.fold_left
         (fun acc (d1, d2) ->
           let c1 = get_coord (add_v2 coord d1) grid in
           let c2 = get_coord (add_v2 coord d2) grid in
           match c1, c2 with
           | Some 'M', Some 'S' | Some 'S', Some 'M' -> acc + 1
           | _ -> acc)
         0
  | _ -> 0
;;

let do_part1 input =
  let count = ref 0 in
  input
  |> Array.iteri (fun x row ->
    row |> Array.iteri (fun y _ -> count := !count + search (x, y) input));
  !count
;;

let do_part2 input =
  let count = ref 0 in
  input
  |> Array.iteri (fun x row ->
    row
    |> Array.iteri (fun y _ ->
      count := !count + if count_mas (x, y) input >= 2 then 1 else 0));
  !count
;;

module Test = struct
  open Alcotest

  let%test "s1 sample" =
    let res = Common.resource_file "day_4/sample.txt" |> parse_input |> do_part1 in
    (check int) "s1 sample" 18 res
  ;;

  let%test "s1 puzzle" =
    let res = Common.resource_file "day_4/puzzle.txt" |> parse_input |> do_part1 in
    (check int) "s1 puzzle" 2578 res
  ;;

  let%test "s2 sample" =
    let res = Common.resource_file "day_4/sample.txt" |> parse_input |> do_part2 in
    (check int) "s2 sample" 9 res
  ;;

  let%test "s2 puzzle" =
    let res = Common.resource_file "day_4/puzzle.txt" |> parse_input |> do_part2 in
    (check int) "s2 puzzle" 1972 res
  ;;
end
