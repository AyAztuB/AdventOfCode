let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_input f =
    let rec _get_line out =
        match input_line f with
        line -> _get_line (line :: out)
        | exception End_of_file -> List.rev out |> Array.of_list
    in _get_line []

let solve f =
    let rec _solve part1 part2 =
        match input_line f with
        line -> _solve (part1) (part2)
        | exception End_of_file -> (part1, part2)
    in _solve 0 0

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
