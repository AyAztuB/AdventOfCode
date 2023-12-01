let file = "input"

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
    print_endline (Format.sprintf "part1: %d\npart2: %d" (fst res) (snd res))
