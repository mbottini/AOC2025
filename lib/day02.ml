open Angstrom
open Parser_utils
open Prelude

let parse_entry = both digits (char '-' *> digits >>| ( + ) 1) >>| uncurry range
let parse_input_line = sep_by (char ',') parse_entry
let double_regex = Str.regexp {|\(.*\)\1$|}
let repeat_regex = Str.regexp {|\(.*\)\1+$|}
let is_double n = string_of_int n |> regex_match double_regex
let is_repeat n = string_of_int n |> regex_match repeat_regex

let parse lines =
  List.of_seq lines |> List.hd
  |> run_parser parse_input_line
  |> List.to_seq |> Seq.concat

let part_1 lines =
  parse lines |> Seq.filter is_double |> Seq.fold_left ( + ) 0 |> string_of_int

let part_2 lines =
  parse lines |> Seq.filter is_repeat |> Seq.fold_left ( + ) 0 |> string_of_int

let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
