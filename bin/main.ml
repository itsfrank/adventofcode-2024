open Aoc_common

let () =
  print_endline "Advent of Code 2024";
  (* day1 *)
  print_endline "  day1: ";
  print_endline "    sample: ";
  Printf.printf
    "      part1: %d\n"
    (Day_1.do_part1 (Common.resource_file "day_1/sample.txt"));
  Printf.printf
    "      part1: %d\n"
    (Day_1.do_part2 (Common.resource_file "day_1/sample.txt"));
  print_endline "    puzzle: ";
  Printf.printf
    "      part1: %d\n"
    (Day_1.do_part1 (Common.resource_file "day_1/puzzle.txt"));
  Printf.printf
    "      part1: %d\n"
    (Day_1.do_part2 (Common.resource_file "day_1/puzzle.txt"))
;;
