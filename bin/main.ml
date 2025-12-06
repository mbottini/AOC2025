open AOC2025.Main_utils

let function_table =
  [|
    AOC2025.Day01.impl;
    AOC2025.Day02.impl;
    AOC2025.Day03.impl;
    AOC2025.Day04.impl;
    AOC2025.Day05.impl;
    AOC2025.Day06.impl;
  |]

let rec main args =
  match args with
  | [| _; day_s; part_s; filename |] -> (
      let day = parse_day day_s in
      let part = parse_part part_s in
      let lines =
        create_channel filename |> In_channel.input_lines |> List.to_seq
      in
      match part with
      | 1 -> function_table.(day).p1 lines |> print_endline
      | 2 -> function_table.(day).p2 lines |> print_endline
      | _ -> failwith "part error")
  | arr when Array.length arr = 3 -> main (Array.append args [| "-" |])
  | _ -> prerr_endline "Usage: ./prog -d<num> -p<num> <filename>"

let () = main Sys.argv
