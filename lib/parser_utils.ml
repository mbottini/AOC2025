open Angstrom

let digits =
  let is_digit c = match c with '0' .. '9' -> true | _ -> false in
  let f cs = List.to_seq cs |> String.of_seq |> int_of_string in
  many (satisfy is_digit) >>| f
