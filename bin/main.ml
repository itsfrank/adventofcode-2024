open Aoc_common

let () =
  print_endline "Advent of Code 2024";
  (* day_1 *)
  print_endline "  day_1: ";
  let day_1_sample = Common.resource_file "day_1/sample.txt" in
  let day_1_puzzle = Common.resource_file "day_1/puzzle.txt" in
  print_endline "    sample: ";
  Printf.printf "      part1: %d\n" (Day_1.do_part1 day_1_sample);
  Printf.printf "      part2: %d\n" (Day_1.do_part2 day_1_sample);
  print_endline "    puzzle: ";
  Printf.printf "      part1: %d\n" (Day_1.do_part1 day_1_puzzle);
  Printf.printf "      part2: %d\n" (Day_1.do_part2 day_1_puzzle);
  (* day_2 *)
  print_endline " --- ";
  print_endline "  day_2: ";
  let day_2_sample = Common.resource_file "day_2/sample.txt" |> Day_2.parse_input in
  let day_2_puzzle = Common.resource_file "day_2/puzzle.txt" |> Day_2.parse_input in
  print_endline "    sample: ";
  Printf.printf "      part1: %d\n" (Day_2.do_part1 day_2_sample);
  Printf.printf "      part2: %d\n" (Day_2.do_part2 day_2_sample);
  print_endline "    puzzle: ";
  Printf.printf "      part1: %d\n" (Day_2.do_part1 day_2_puzzle);
  Printf.printf "      part2: %d\n" (Day_2.do_part2 day_2_puzzle)
;;
