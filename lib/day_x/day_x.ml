open Aoc_common
(* open Containers *)

(* converts input into two lists *)
let parse_input _filename = 1
let do_part1 _input = 1
let do_part2 _input = 1

module Test = struct
  open Alcotest

  let%test "parse input" =
    let data = Common.resource_file "day_x/sample.txt" |> parse_input in
    (check int) "c" 1 data
  ;;

  let%test "s1 sample" =
    let res = Common.resource_file "day_x/sample.txt" |> parse_input |> do_part1 in
    (check int) "s1 sample" 1 res
  ;;

  let%test "s1 main" =
    let res = Common.resource_file "day_x/puzzle.txt" |> parse_input |> do_part1 in
    (check int) "s1 puzzle" 1 res
  ;;

  let%test "s2 sample" =
    let res = Common.resource_file "day_x/sample.txt" |> parse_input |> do_part2 in
    (check int) "s2 sample" 1 res
  ;;

  let%test "s2 main" =
    let res = Common.resource_file "day_x/puzzle.txt" |> parse_input |> do_part2 in
    (check int) "s2 puzzle" 1 res
  ;;
end
