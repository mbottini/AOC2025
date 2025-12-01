let create_channel filename =
  match filename with
  | "-" -> In_channel.stdin
  | _ -> In_channel.open_text filename

let parse_part s =
  let part_regexp = Str.regexp {|-p\([0-9]+\)|} in
  if
    Str.string_match part_regexp s 0
    && String.length (Str.matched_group 0 s) == String.length s
  then Str.matched_group 1 s |> int_of_string
  else failwith "parse error"

let parse_day s =
  let part_regexp = Str.regexp {|-d\([0-9]+\)|} in
  if
    Str.string_match part_regexp s 0
    && String.length (Str.matched_group 0 s) == String.length s
  then Str.matched_group 1 s |> int_of_string |> fun x -> x - 1
  else failwith "parse error"

type day_implementation = {
  p1 : string Seq.t -> string;
  p2 : string Seq.t -> string;
}
