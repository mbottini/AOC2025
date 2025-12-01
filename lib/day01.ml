open Angstrom
open Parser_utils
open Prelude

let parse_left = char 'L' *> digits >>| ( ~- )
let parse_right = char 'R' *> digits
let parse_line = parse_left <|> parse_right
let run_parser line = parse_string ~consume:All parse_line line |> Result.get_ok

let dial_states start xs =
  let go curr x = (((curr + x) mod 100) + 100) mod 100 in
  Seq.scan go start xs

let rec separate_nums n =
  if abs n <= 99 then Seq.return n
  else
    let sign = if n < 0 then -1 else 1 in
    Seq.cons (99 * sign) (separate_nums (n - (99 * sign)))

let abs_states start xs = Seq.flat_map separate_nums xs |> Seq.scan ( + ) start
let clamp100 n = (n / 100) - if n < 0 then 1 else 0

let count_ticks n1 n2 =
  match (n1, n2) with
  | x, _ when x mod 100 = 0 -> 0
  | _, x when x mod 100 = 0 -> 1
  | _ -> if clamp100 n1 <> clamp100 n2 then 1 else 0

let part_1 lines =
  Seq.map run_parser lines |> dial_states 50
  |> Seq.filter (( = ) 0)
  |> Seq.length |> string_of_int

let part_2 lines =
  Seq.map run_parser lines |> abs_states 50 |> pairwise
  |> Seq.map (uncurry count_ticks)
  |> Seq.fold_left ( + ) 0 |> string_of_int

let impl : Main_utils.day_implementation = { p1 = part_1; p2 = part_2 }
