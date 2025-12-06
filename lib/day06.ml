open Angstrom
open Parser_utils
open Prelude
module IntSet = Set.Make (Int)

type op = Add | Mul

let apply_op op nums =
  let f, init = match op with Add -> (( + ), 0) | Mul -> (( * ), 1) in
  Seq.fold_left f init nums

let parse_op = char '+' *> return Add <|> char '*' *> return Mul
let is_space c = match c with ' ' | '\t' -> true | _ -> false
let parse_number_line = sep_by (many1 (satisfy is_space)) digits
let parse_op_line = sep_by (many1 (satisfy is_space)) parse_op

let parse_file_p1 lines =
  match Seq.map String.trim lines |> List.of_seq |> List.rev with
  | op_line :: rest ->
      ( run_parser parse_op_line op_line,
        List.map (run_parser parse_number_line) rest |> List.rev )
  | _ -> failwith "parse error"

let part_1 lines =
  let ops, xss = parse_file_p1 lines in
  Seq.map2 apply_op (List.to_seq ops)
    (List.map List.to_seq xss |> List.to_seq |> Seq.transpose)
  |> Seq.fold_left ( + ) 0 |> string_of_int

let replace c1 c2 s = String.map (fun c -> if c = c1 then c2 else c) s

let resolve_nums strings =
  Seq.map String.to_seq strings
  |> Seq.transpose
  |> Seq.map (Seq.filter (( <> ) ' ') >> String.of_seq)
  |> Seq.filter (( <> ) "")
  |> Seq.map (String.trim >> int_of_string)

let find_spaces s =
  let rec aux idx acc xs =
    match xs with
    | [] -> IntSet.of_list acc
    | x :: xs ->
        if x = ' ' then aux (idx + 1) (idx :: acc) xs else aux (idx + 1) acc xs
  in
  aux 0 [] (String.to_seq s |> List.of_seq)

let find_column_breaks lines =
  List.map find_spaces lines |> List.to_seq |> foldl1 IntSet.inter
  |> IntSet.to_list |> List.cons (-1) |> List.to_seq |> pairwise
  |> Seq.map (uncurry (Fun.flip ( - )))
  |> List.of_seq

let divvy counts xs =
  let rec aux acc ns xs =
    match (ns, xs) with
    | [], [] -> List.rev acc
    | [], xs -> xs :: acc |> List.rev
    | n :: ns, _ -> aux (List.take n xs :: acc) ns (List.drop n xs)
  in
  aux [] counts xs

let split_string counts s =
  String.to_seq s |> List.of_seq |> divvy counts
  |> List.map (List.to_seq >> String.of_seq)

let parse_grid lines =
  let counts = find_column_breaks lines in
  List.map (split_string counts) lines
  |> List.map List.to_seq |> List.to_seq |> Seq.transpose
  |> Seq.map resolve_nums

let parse_file_p2 lines =
  match List.of_seq lines |> List.rev with
  | op_line :: rest ->
      ( run_parser parse_op_line (String.trim op_line),
        parse_grid (List.rev rest) )
  | _ -> failwith "parse error"

let part_2 lines =
  let ops, xss = parse_file_p2 lines in
  Seq.map2 apply_op (List.to_seq ops) xss
  |> Seq.fold_left ( + ) 0 |> string_of_int

let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
