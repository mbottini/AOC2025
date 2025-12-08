open Angstrom
open Parser_utils
open Prelude
module UnionFindMap = UnionFind.Make (UnionFind.StoreMap)

type vector = { x : int; y : int; z : int }

let cmp_vector { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  compare (x1, y1, z1) (x2, y2, z2)

module Vector = struct
  type t = vector

  let compare = cmp_vector
end

module VectorMap = Map.Make (Vector)
module VectorSet = Set.Make (Vector)
module VectorVectorSet = Set.Make (VectorSet) (* aaaaaaaaaa *)
module FrequencyMap = Frequency_map.Make (VectorMap)

let distance { x = x1; y = y1; z = z1 } { x = x2; y = y2; z = z2 } =
  let square x = x * x in
  square (x2 - x1) + square (y2 - y1) + square (z2 - z1) |> float_of_int |> sqrt

let vector_parser =
  sep_by (char ',') digits >>| tuple3 >>| fun (x, y, z) -> { x; y; z }

let parse_line = run_parser vector_parser

let parse_file lines =
  let st = UnionFindMap.new_store () in
  let mp =
    Seq.map parse_line lines
    |> Seq.map (fun v -> (v, UnionFindMap.make st (VectorSet.singleton v)))
    |> VectorMap.of_seq
  in
  (st, mp)

let brute_force_pathing mp =
  let vectors = VectorMap.to_seq mp |> Seq.map fst |> List.of_seq in
  Seq.product (List.to_seq vectors) (List.to_seq vectors)
  |> Seq.filter (fun (v1, v2) -> cmp_vector v1 v2 = -1)
  |> Seq.map (fun (v1, v2) -> (distance v1 v2, (v1, v2)))
  |> List.of_seq
  |> List.sort (fun (d1, _) (d2, _) -> compare d1 d2)
  |> List.map snd

let peek f xs =
  List.iter f xs;
  xs

let part_1 n (st, mp) =
  let union v1 v2 = UnionFindMap.merge st VectorSet.union v1 v2 |> ignore in
  let find v = VectorMap.find v mp in
  let pairs = brute_force_pathing mp |> List.take n in
  List.iter (fun (v1, v2) -> union (find v1) (find v2)) pairs;
  VectorMap.to_seq mp
  |> Seq.map (snd >> UnionFindMap.get st)
  |> VectorVectorSet.of_seq |> VectorVectorSet.to_list
  |> List.sort (fun s1 s2 ->
      compare (VectorSet.cardinal s1) (VectorSet.cardinal s2))
  |> List.rev |> List.take 3
  |> List.map VectorSet.cardinal
  |> List.fold_left ( * ) 1 |> string_of_int

let part_2 (st, mp) =
  let num_boxes = VectorMap.cardinal mp in
  let union v1 v2 =
    UnionFindMap.merge st VectorSet.union v1 v2 |> UnionFindMap.get st
  in
  let find v = VectorMap.find v mp in
  let pairs = brute_force_pathing mp in
  let go _ (v1, v2) =
    union (find v1) (find v2) |> ignore;
    (v1, v2)
  in
  scanl1 go (List.to_seq pairs)
  |> Seq.drop_while (fun (v1, _) ->
      find v1 |> UnionFindMap.get st |> VectorSet.cardinal <> num_boxes)
  |> Seq.uncons |> Option.get |> fst
  |> fun (v1, v2) -> v1.x * v2.x |> string_of_int

let impl : Main_utils.day_implementation =
  { p1 = parse_file >> part_1 1000; p2 = parse_file >> part_2 }
