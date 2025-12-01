open Aoc_2025.Main_utils

let function_table = [| Aoc_2025.Day01.impl |]

let () =
  match Sys.argv with
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
  | _ -> prerr_endline "Usage: ./prog -d<num> -p<num> <filename>"
