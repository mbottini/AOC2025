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
