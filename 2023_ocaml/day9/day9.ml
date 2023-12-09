let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let line_to_list_of_nums line =
    line
    |> String.split_on_char ' '
    |> List.map int_of_string

let compute_delta line =
    let rec _compute l out =
        match l with
        | a :: b :: c -> _compute (b::c) ((b-a)::out)
        | _ -> List.rev out
    in _compute line []

let rec eval line part =
    if List.for_all ((=) 0) line then 0 else
    let diff = eval (compute_delta line) part in
    if part = 1 then (List.hd (List.rev line) + diff) else
    List.hd line - diff

let solve f =
    let rec _solve part1 part2 =
        match input_line f with
        line ->
            let nums = line_to_list_of_nums line in
            _solve (part1 + eval nums 1) (part2 + eval nums 2)
        | exception End_of_file -> (part1, part2)
    in _solve 0 0

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
