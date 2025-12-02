open Str

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
let range x y = Seq.iterate (( + ) 1) x |> Seq.take_while (( > ) y)
let string_of_list xs = List.to_seq xs |> String.of_seq
let regex_match regex s = string_match regex s 0
