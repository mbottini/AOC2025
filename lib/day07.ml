open Manhattan_graph
open Prelude

type terrain = Floor | Splitter | Start
type terrain_node = terrain node

module Node = struct
  type t = terrain_node

  let compare = node_cmp
end

module NodeSet = Set.Make (Node)
module NodeMap = Map.Make (Node)

let parse_terrain c =
  match c with
  | '.' -> Floor
  | '^' -> Splitter
  | 'S' -> Start
  | _ -> failwith "parse error"

let parse_graph lines =
  Seq.map (String.to_seq >> Seq.map parse_terrain >> List.of_seq) lines
  |> List.of_seq |> create_graph |> Lazy.force

let find_start node_opt =
  all_nodes node_opt
  |> Seq.filter (fun n -> n.data = Start)
  |> Seq.uncons |> Option.map fst

let tachyon_step_p1 ns =
  let go n =
    match trav S (Some n) with
    | Some n as n_opt when n.data = Splitter ->
        [ W; E ]
        |> List.concat_map (fun dir -> trav dir n_opt |> Option.to_list)
        |> fun xs -> (1, xs)
    | Some n -> (0, [ n ])
    | None -> (0, [])
  in
  let split_count, nodes =
    NodeSet.to_list ns |> List.map go |> List.to_seq |> Seq.unzip
  in
  ( Seq.fold_left ( + ) 0 split_count,
    Seq.concat_map List.to_seq nodes |> NodeSet.of_seq )

let memoizer f =
  let tbl = ref NodeMap.empty in

  let rec g n =
    match NodeMap.find_opt n !tbl with
    | Some v -> v
    | None ->
        let v = f g n in
        tbl := NodeMap.add n v !tbl;
        v
  in
  g

let count_timelines_helper self n =
  match trav S (Some n) with
  | None -> 1
  | Some n as n_opt when n.data = Splitter ->
      [ E; W ]
      |> List.concat_map (fun dir -> trav dir n_opt |> Option.to_list)
      |> List.map self |> List.fold_left ( + ) 0
  | Some n -> self n

let count_timelines = memoizer count_timelines_helper

let part_1 lines =
  parse_graph lines |> find_start |> Option.get |> fun n ->
  (0, NodeSet.singleton n)
  |> Seq.iterate (snd >> tachyon_step_p1)
  |> Seq.take_while (snd >> NodeSet.is_empty >> not)
  |> Seq.map fst |> Seq.fold_left ( + ) 0 |> string_of_int

let part_2 lines =
  parse_graph lines |> find_start |> Option.get |> count_timelines
  |> string_of_int

let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
