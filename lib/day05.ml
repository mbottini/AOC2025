open Angstrom
open Parser_utils
open Prelude

type rng = { start : int; stop : int }
type tree = { data : rng; left : tree option; right : tree option }

let between a x b = a <= x && x <= b

let is_overlapping { start = x1; stop = y1 } { start = x2; stop = y2 } =
  between x1 x2 y1 || between x1 y2 y1 || between x2 x1 y2 || between x2 y1 y2

(* Assumes that they are contiguous! Check first! *)
let merge_rngs { start = x1; stop = y1 } { start = x2; stop = y2 } =
  { start = min x1 x2; stop = max y1 y2 }

let cmp x rng = if x < rng.start then -1 else if x >= rng.stop then 1 else 0

let cmp_tree t1 t2 =
  if t1.stop < t2.start then -1 else if t2.stop < t1.start then 1 else 0

let rec query_tree tree_opt x =
  match tree_opt with
  | Some t -> (
      match cmp x t.data with
      | -1 -> query_tree t.left x
      | 1 -> query_tree t.right x
      | _ -> true)
  | None -> false

let rec insert_rng t_opt r =
  match t_opt with
  | None -> Some { data = r; left = None; right = None }
  | Some t when t.data.start > r.stop ->
      Some { t with left = insert_rng t.left r }
  | Some t when t.data.stop < r.start ->
      Some { t with right = insert_rng t.right r }
  | Some t -> Some { t with data = merge_rngs t.data r }

let rec tree_to_seq t_opt =
  match t_opt with
  | None -> Seq.empty
  | Some t ->
      [ tree_to_seq t.left; Seq.return t.data; tree_to_seq t.right ]
      |> List.to_seq |> Seq.concat

let range_size { start; stop } = stop - start

let parse_range =
  both digits (char '-' *> digits >>| ( + ) 1) >>| fun (start, stop) ->
  { start; stop }

let parse_seq lines =
  let ranges =
    Seq.take_while (String.length >> ( <> ) 0) lines
    |> Seq.map (run_parser parse_range)
    |> List.of_seq
  in
  let ingredients =
    Seq.drop_while (String.length >> ( <> ) 0) lines
    |> Seq.drop 1 |> Seq.map int_of_string |> List.of_seq
  in
  (List.fold_left insert_rng None ranges, ingredients)

let part_1 lines =
  let range_tree, ingredients = parse_seq lines in
  List.filter (query_tree range_tree) ingredients
  |> List.length |> string_of_int

let part_2 lines =
  let range_tree, _ = parse_seq lines in
  tree_to_seq range_tree |> Seq.map range_size |> Seq.fold_left ( + ) 0
  |> string_of_int

let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
