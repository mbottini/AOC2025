open Str

let ( >> ) f g x = g (f x)

let pairwise xs =
  let rec aux curr ys =
    match ys with
    | Seq.Nil -> Seq.Nil
    | Seq.Cons (y, ys') -> Seq.Cons ((curr, y), fun () -> aux y (ys' ()))
  in
  match xs () with
  | Seq.Nil -> Seq.empty
  | Seq.Cons (x, xs') -> fun () -> aux x (xs' ())

let uncurry f (x, y) = f x y
let uncurry3 f (x, y, z) = f x y z
let range x y = Seq.iterate (( + ) 1) x |> Seq.take_while (( > ) y)
let string_of_list xs = List.to_seq xs |> String.of_seq
let regex_match regex s = string_match regex s 0
let max_list init xs = List.fold_left max init xs

let window n xs =
  let rec aux acc xs =
    match List.take n xs with
    | ys when List.length ys = n -> aux (ys :: acc) (List.tl xs)
    | _ -> List.rev acc
  in
  aux [] xs

let triplewise xs = window 3 xs

let tuple3 xs =
  match xs with
  | [ x; y; z ] -> (x, y, z)
  | _ -> failwith "Something went very wrong here"

let zip3 xs ys zs =
  let rec aux acc xs ys zs =
    match (xs, ys, zs) with
    | x :: xs, y :: ys, z :: zs -> aux ((x, y, z) :: acc) xs ys zs
    | _ -> List.rev acc
  in
  aux [] xs ys zs
