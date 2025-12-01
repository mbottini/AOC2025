open Angstrom
open Parser_utils

let create_channel filename =
  match filename with
  | "-" -> In_channel.stdin
  | _ -> In_channel.open_text filename

let parse_part s =
  let p = string "-p" *> digits in
  run_parser p s

let parse_day s =
  let p = string "-d" *> digits >>| fun x -> x - 1 in
  run_parser p s

type day_implementation = {
  p1 : string Seq.t -> string;
  p2 : string Seq.t -> string;
}
