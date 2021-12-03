(* split the first element and the rest of the sequence
   fails on sequences with length 0 *)
let uncons seq =
  match seq () with Seq.Cons (v, seq') -> (v, seq') | Seq.Nil -> failwith "no"

(* sum of 3 element sliding window
   fails on sequences with length 0 or 1. *)
let sum_triples seq =
  let a, seq' = uncons seq in
  let b, seq'' = uncons seq' in
  let rec aux s a b () =
    match s () with
    | Seq.Cons (c, s') -> Seq.Cons (a + b + c, aux s' b c)
    | Seq.Nil -> Seq.Nil
  in
  aux seq'' a b

(* sequence of (current, previous) values
   fails on sequences with length 0 *)
let pair seq =
  let a, seq' = uncons seq in
  let rec aux s prev () =
    match s () with
    | Seq.Cons (v, s') -> Seq.Cons ((v, prev), aux s' v)
    | Seq.Nil -> Seq.Nil
  in
  aux seq' a

(* count cases where current is greater than previous *)
let count_increasing pairs =
  Seq.fold_left (fun acc (v, prev) -> acc + if v > prev then 1 else 0) 0 pairs

(* input a file line by line, converting each to an int.
   No error checking on number conversion *)
let rec input file () =
  try Seq.Cons (input_line file |> int_of_string, input file)
  with End_of_file -> Seq.Nil

let increasing_values fname =
  let file = open_in fname in
  Fun.protect
    ~finally:(fun () -> close_in file)
    (fun () ->
      let nums = input file in
      let pairs = pair nums in
      count_increasing pairs)

let increasing_windows fname =
  let file = open_in fname in
  Fun.protect
    ~finally:(fun () -> close_in file)
    (fun () ->
      let nums = input file in
      let sums = sum_triples nums in
      let pairs = pair sums in
      count_increasing pairs)
