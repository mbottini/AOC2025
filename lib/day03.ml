open Prelude

let parse_line line =
  String.to_seq line
  |> Seq.map (fun c -> int_of_char c - int_of_char '0')
  |> List.of_seq

let max_joltage p ns =
  let rec aux acc p ns =
    if p = 0 then acc
    else
      let max_digit = List.rev ns |> List.drop (p - 1) |> max_list (-1) in
      let index = List.find_index (( = ) max_digit) ns |> Option.get in
      aux ((10 * acc) + max_digit) (p - 1) (List.drop (index + 1) ns)
  in
  aux 0 p ns

let part_n p lines =
  Seq.map parse_line lines
  |> Seq.map (max_joltage p)
  |> Seq.fold_left ( + ) 0 |> string_of_int

let part_1 = part_n 2
let part_2 = part_n 12
let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
