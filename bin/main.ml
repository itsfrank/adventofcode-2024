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
  Printf.printf "      part2: %d\n" (Day_2.do_part2 day_2_puzzle);
  (* day_3 *)
  print_endline " --- ";
  print_endline "  day_3: ";
  let day_3_sample = Common.resource_file "day_3/sample.txt" |> Day_3.parse_input in
  let day_3_puzzle = Common.resource_file "day_3/puzzle.txt" |> Day_3.parse_input in
  print_endline "    sample: ";
  Printf.printf "      part1: %d\n" (Day_3.do_part1 day_3_sample);
  Printf.printf "      part2: %d\n" (Day_3.do_part2 day_3_sample);
  print_endline "    puzzle: ";
  Printf.printf "      part1: %d\n" (Day_3.do_part1 day_3_puzzle);
  Printf.printf "      part2: %d\n" (Day_3.do_part2 day_3_puzzle);
  (* day_4 *)
  print_endline " --- ";
  print_endline "  day_4: ";
  let day_4_sample = Common.resource_file "day_4/sample.txt" |> Day_4.parse_input in
  let day_4_puzzle = Common.resource_file "day_4/puzzle.txt" |> Day_4.parse_input in
  print_endline "    sample: ";
  Printf.printf "      part1: %d\n" (Day_4.do_part1 day_4_sample);
  Printf.printf "      part2: %d\n" (Day_4.do_part2 day_4_sample);
  print_endline "    puzzle: ";
  Printf.printf "      part1: %d\n" (Day_4.do_part1 day_4_puzzle);
  Printf.printf "      part2: %d\n" (Day_4.do_part2 day_4_puzzle);
  (* day_5 *)
  print_endline " --- ";
  print_endline "  day_5: ";
  let day_5_sample = Common.resource_file "day_5/sample.txt" |> Day_5.parse_input in
  let day_5_puzzle = Common.resource_file "day_5/puzzle.txt" |> Day_5.parse_input in
  print_endline "    sample: ";
  Printf.printf "      part1: %d\n" (Day_5.do_part1 day_5_sample);
  Printf.printf "      part2: %d\n" (Day_5.do_part2 day_5_sample);
  print_endline "    puzzle: ";
  Printf.printf "      part1: %d\n" (Day_5.do_part1 day_5_puzzle);
  Printf.printf "      part2: %d\n" (Day_5.do_part2 day_5_puzzle)
;;

(* replace cmd: `s/_x/_n/g` otherwise you replace the x in '.txt' *)

(* ;(* day_x *) *)
(* print_endline " --- "; *)
(* print_endline "  day_x: "; *)
(* let day_x_sample = Common.resource_file "day_x/sample.txt" |> Day_x.parse_input in *)
(* let day_x_puzzle = Common.resource_file "day_x/puzzle.txt" |> Day_x.parse_input in *)
(* print_endline "    sample: "; *)
(* Printf.printf "      part1: %d\n" (Day_x.do_part1 day_x_sample); *)
(* Printf.printf "      part2: %d\n" (Day_x.do_part2 day_x_sample); *)
(* print_endline "    puzzle: "; *)
(* Printf.printf "      part1: %d\n" (Day_x.do_part1 day_x_puzzle); *)
(* Printf.printf "      part2: %d\n" (Day_x.do_part2 day_x_puzzle) *)
