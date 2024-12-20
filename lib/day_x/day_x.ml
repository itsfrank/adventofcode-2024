open Aoc_common
(* open Containers *)

let parse_input_impl _lines = 1
let parse_input filename = filename |> Common.read_lines |> parse_input_impl
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

  let%test "s1 puzzle" =
    let res = Common.resource_file "day_x/puzzle.txt" |> parse_input |> do_part1 in
    (check int) "s1 puzzle" res res
  ;;

  let%test "s2 sample" =
    let res = Common.resource_file "day_x/sample.txt" |> parse_input |> do_part2 in
    (check int) "s2 sample" 1 res
  ;;

  let%test "s2 puzzle" =
    let res = Common.resource_file "day_x/puzzle.txt" |> parse_input |> do_part2 in
    (check int) "s2 puzzle" res res
  ;;
end
