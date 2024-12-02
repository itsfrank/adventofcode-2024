module Common = struct
  let read_lines ?(skip_blank_lines = true) filename : string list =
    let in_channel = open_in filename in
    let rec read_lines_helper acc =
      try
        let line = input_line in_channel in
        if line = "" && skip_blank_lines
        then read_lines_helper acc
        else read_lines_helper (line :: acc)
      with
      | End_of_file -> acc
    in
    let lines = read_lines_helper [] in
    close_in in_channel;
    List.rev lines
  ;;

  let split_whitespace s = String.split_on_char ' ' s |> List.filter (fun s -> s <> "")

  let rec pop_n n list : string list =
    match list, n with
    | _, 0 -> list
    | _ :: tl, _ -> pop_n (n - 1) tl
    | [], _ -> []
  ;;

  let resource_file file =
    let rec helper current_dir =
      if Sys.file_exists (Filename.concat current_dir "resources")
         && Sys.is_directory (Filename.concat current_dir "resources")
      then Some current_dir
      else (
        let parent_dir = Filename.dirname current_dir in
        if parent_dir <> current_dir
        then (* If there is a parent directory *)
          helper parent_dir
        else None)
    in
    let res_dir =
      match helper (Sys.getcwd ()) with
      | Some v -> v
      | None ->
        failwith (Format.sprintf "no resource dir found above '%s'" (Sys.getcwd ()))
    in
    Filename.concat res_dir (Filename.concat "resources" file)
  ;;

  let parse_file ?(on_token = fun _ -> ()) ?(on_line = fun _ -> ()) file_path =
    let data = read_lines file_path |> List.map (fun l -> l, split_whitespace l) in
    data
    |> List.iter (fun l ->
      let line, tokens = l in
      List.iter on_token tokens;
      on_line line)
  ;;
end

module Test = struct
  open Alcotest

  let%test "test res dir" =
    let s = Common.resource_file "aoc_common/hello.txt" in
    (check bool) "pass" true (String.ends_with ~suffix:"resources/aoc_common/hello.txt" s)
  ;;

  let%test "test read lines" =
    let lines = Common.resource_file "aoc_common/hello.txt" |> Common.read_lines in
    (check (list string)) "eq" [ "hello"; "world" ] lines
  ;;

  let%test "test read lines skip blank" =
    let lines =
      Common.resource_file "aoc_common/hello_blankline.txt" |> Common.read_lines
    in
    (check (list string)) "eq" [ "hello"; "world" ] lines
  ;;

  let%test "test pop lines" =
    let lines =
      Common.resource_file "aoc_common/hello.txt" |> Common.read_lines |> Common.pop_n 1
    in
    (check (list string)) "eq" [ "world" ] lines
  ;;

  let%test "test split whitespace" =
    let line =
      Common.resource_file "aoc_common/ws_delimited.txt"
      |> Common.read_lines
      |> List.hd
      |> Common.split_whitespace
    in
    (check (list string)) "ws delim" [ "hello"; "world"; "space"; "delim" ] line
  ;;

  let%test "test parse file" =
    let res = ref [] in
    let curr_str = ref "" in
    Common.resource_file "aoc_common/letter_lines.txt"
    |> Common.parse_file
         ~on_line:(fun _ ->
           res := !res @ [ !curr_str ];
           curr_str := "")
         ~on_token:(fun t -> curr_str := !curr_str ^ t);
    (check (list string)) "parse file" [ "qwer"; "ty"; "ui"; "opa" ] !res
  ;;
end
