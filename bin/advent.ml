let day1 fname =
  let part1 = Lib.Day1.increasing_values fname in
  let part2 = Lib.Day1.increasing_windows fname in
  print_int part1;
  print_newline ();
  print_int part2;
  print_newline ()

let day2 fname =
  let r, destroy = Lib.Day2.run fname in
  print_int r;
  print_newline ();
  destroy ()

let () =
  match Sys.argv.(1) with
  | "day1" ->
      let fname = Sys.argv.(2) in
      day1 fname
  | "day2" ->
      let fname = Sys.argv.(2) in
      day2 fname
  | _ -> print_endline "no day selected"
