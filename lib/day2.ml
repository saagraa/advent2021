type state = { horizontal : int; depth : int; aim : int }

let print_state state =
  Printf.printf "{ horizontal: %d; depth: %d; aim: %d; mul: %d }\n"
    state.horizontal state.depth state.aim
    (state.horizontal * state.depth)

type action = [ `Forward of int | `Down of int | `Up of int ]

let do_action action state =
  match action with
  | `Forward x ->
      {
        state with
        horizontal = state.horizontal + x;
        depth = state.depth + (state.aim * x);
      }
  | `Down x -> { state with aim = state.aim + x }
  | `Up x -> { state with aim = state.aim - x }

let action_of_string s =
  match String.split_on_char ' ' s with
  | [ cmd; arg ] -> (
      let x = int_of_string arg in
      match cmd with
      | "forward" -> `Forward x
      | "down" -> `Down x
      | "up" -> `Up x
      | s -> failwith s)
  | _ -> failwith "no"

let read_file fname =
  let ev, send = Note.E.create () in
  let rec f ic =
    try
      input_line ic |> action_of_string |> send;
      f ic
    with End_of_file -> ()
  in
  let exec () =
    let ic = open_in fname in
    Fun.protect ~finally:(fun () -> close_in ic) (fun () -> f ic)
  in
  (ev, exec)

let run fname =
  let state, exec =
    Note.S.fix { horizontal = 0; depth = 0; aim = 0 } (fun state ->
        let actions, exec = read_file fname in
        let state' =
          Note.S.accum (Note.S.value state) (Note.E.map do_action actions)
        in
        (state', (state', exec)))
  in
  let obs = Note.S.log ~now:true state print_state in
  Note.Logr.hold obs;
  exec ();
  let result = Note.S.value state |> (fun s -> s.horizontal * s.depth) in
  result, (fun () -> Note.Logr.destroy obs)