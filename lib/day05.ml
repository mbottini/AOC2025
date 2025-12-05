open Angstrom
open Parser_utils
open Prelude

type rng = { start : int; stop : int }
type tree = { data : rng; left : tree option; right : tree option }

let between a x b = a <= x && x <= b

(* Assumes that the second range's start must be after the first range's start. *)
let is_overlapping { start = x1; stop = y1 } { start = x2; _ } =
  between x1 x2 y1

(* Assumes that they are contiguous! Check first! *)
let merge_rngs { start = x1; stop = y1 } { start = x2; stop = y2 } =
  { start = min x1 x2; stop = max y1 y2 }

let cmp x rng = if x < rng.start then -1 else if x >= rng.stop then 1 else 0

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

let combine_ranges rngs =
  let cmp_ranges r1 r2 = Int.compare r1.start r2.start in
  let rec aux acc xs =
    match (xs, acc) with
    | [], _ -> List.rev acc
    | x :: xs, a :: acc' when is_overlapping a x ->
        aux (merge_rngs a x :: acc') xs
    | x :: xs, _ -> aux (x :: acc) xs
  in
  match List.sort cmp_ranges rngs with [] -> [] | x :: xs -> aux [ x ] xs

let split_middle xs =
  let rec aux acc turtle rabbit =
    match rabbit with
    | [] | _ :: [] -> (List.rev acc, List.hd turtle, List.tl turtle)
    | _ :: _ :: rest -> aux (List.hd turtle :: acc) (List.tl turtle) rest
  in
  aux [] xs xs

let rec create_tree xs =
  match xs with
  | [] -> None
  | _ ->
      let left, middle, right = split_middle xs in
      Some { data = middle; left = create_tree left; right = create_tree right }

let range_size { start; stop } = stop - start

let parse_range =
  both digits (char '-' *> digits >>| ( + ) 1) >>| fun (start, stop) ->
  { start; stop }

let parse_seq lines =
  let ranges =
    Seq.take_while (String.length >> ( <> ) 0) lines
    |> Seq.map (run_parser parse_range)
    |> List.of_seq |> combine_ranges |> create_tree
  in
  let ingredients =
    Seq.drop_while (String.length >> ( <> ) 0) lines
    |> Seq.drop 1 |> Seq.map int_of_string |> List.of_seq
  in
  (ranges, ingredients)

let part_1 lines =
  let range_tree, ingredients = parse_seq lines in
  List.filter (query_tree range_tree) ingredients
  |> List.length |> string_of_int

let part_2 lines =
  let range_tree, _ = parse_seq lines in
  tree_to_seq range_tree |> Seq.map range_size |> Seq.fold_left ( + ) 0
  |> string_of_int

let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
