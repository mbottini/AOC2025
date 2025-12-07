type direction = N | S | E | W

type 'a node = {
  data : 'a;
  coordinates : int * int;
  edges : (direction * 'a node) list Lazy.t;
}

let node_cmp { coordinates = c1; _ } { coordinates = c2; _ } = compare c1 c2

type 'a st = {
  n : 'a node option Lazy.t;
  s : 'a node option Lazy.t;
  w : 'a node option Lazy.t;
  coordinates : int * int;
}

let trav_lazy dir n_opt_lazy =
  let bind = Fun.flip Option.bind in
  let trav dir n = n.edges |> Lazy.force |> List.assoc_opt dir in
  Lazy.map (bind (trav dir)) n_opt_lazy

let inc_col (col, row) = (col + 1, row)

let filter_nones lazy_opts =
  let aux (dir, n_opt_lazy) =
    match Lazy.force n_opt_lazy with Some n -> [ (dir, n) ] | None -> []
  in
  lazy (List.concat_map aux lazy_opts)

let rec inner (st : 'a st) (xs : 'a list) : 'a node option =
  match xs with
  | [] -> None
  | x :: xs ->
      let rec curr =
        lazy
          (Some
             {
               data = x;
               coordinates = st.coordinates;
               edges =
                 filter_nones
                   [
                     (N, st.n);
                     (W, st.w);
                     ( E,
                       lazy
                         (inner
                            {
                              n = trav_lazy E st.n;
                              s = trav_lazy E st.s;
                              w = curr;
                              coordinates = inc_col st.coordinates;
                            }
                            xs) );
                     (S, st.s);
                   ];
             })
      in
      Lazy.force curr

let rec outer row prev xss =
  match xss with
  | [] -> lazy None
  | xs :: xss ->
      let rec curr =
        lazy
          (inner
             {
               n = prev;
               w = lazy None;
               s = outer (row + 1) curr xss;
               coordinates = (0, row);
             }
             xs)
      in
      curr

let create_graph xss = outer 0 (lazy None) xss

let trav dir n_opt =
  let ( >>= ) = Option.bind in
  let aux n = n.edges |> Lazy.force |> List.assoc_opt dir in
  n_opt >>= aux

let all_nodes n_opt =
  let get_row n_opt =
    Seq.iterate (trav E) n_opt
    |> Seq.take_while Option.is_some
    |> Seq.flat_map Option.to_seq
  in
  let rows = Seq.iterate (trav S) n_opt |> Seq.take_while Option.is_some in
  Seq.flat_map get_row rows
