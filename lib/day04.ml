open Prelude

type terrain = Floor | Paper | Removed

let parse_terrain c =
  match c with '.' -> Floor | '@' -> Paper | _ -> failwith "parse error"

let parse_line s = String.to_seq s |> Seq.map parse_terrain |> List.of_seq
let parse_grid lines = List.of_seq lines |> List.map parse_line
let is_paper t = t = Paper

let is_valid_paper (t, ts) =
  match (t, List.filter is_paper ts) with
  | Paper, ts when List.length ts < 4 -> true
  | _ -> false

let part_1 lines =
  parse_grid lines |> neighbors |> List.concat |> List.filter is_valid_paper
  |> List.length |> string_of_int

let mask b t = if b then Removed else t

let p2_step tss =
  let bss = neighbors tss |> List.map (List.map is_valid_paper) in
  List.map2 (List.map2 mask) bss tss

let part_2 lines =
  let tss = parse_grid lines in
  Seq.iterate p2_step tss |> pairwise
  |> Seq.take_while (uncurry ( <> ))
  |> Seq.map snd
  |> Seq.fold_left (Fun.flip Fun.const) tss
  |> List.concat
  |> List.filter (fun t -> t = Removed)
  |> List.length |> string_of_int

let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
