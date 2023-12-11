let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_galaxies line x galaxies =
    let rec _get i out =
        if i >= String.length line then out else
        _get ~++i (if line.[i] = '#' then (x, i)::out else out)
    in _get 0 galaxies

let get_input f =
    let rec _get_line out i empty_rows galaxies =
        match input_line f with
        line ->
            _get_line (line :: out) ~++i
                (if String.for_all
                    (fun ch -> ch = '.')
                    line
                then (i :: empty_rows)
                else empty_rows)
                (get_galaxies line i galaxies)
        | exception End_of_file ->
            (List.rev out |> Array.of_list,
            List.sort compare empty_rows,
            galaxies)
    in _get_line [] 0 [] []

let get_empty_cols input =
    let rec _empty i out =
        if i >= String.length input.(0) then List.sort compare out else
        _empty ~++i
            (if Array.for_all
                (fun str -> str.[i] = '.')
                input
            then (i :: out)
            else out)
    in _empty 0 []

let find_interval p1 p2 empty =
    let start = p1 <<< p2 and last = p1 >>> p2 in
    let rec _find s list i =
        match list with
        | e::q ->
            if e > last then i - s else
            if e <= start then _find ~++s q ~++i else
            _find s q ~++i
        | _ -> i - s
    in _find 0 empty 0

let distance_two_galaxies x1 x2 empty_cols empty_rows expand_ratio =
    if x1 = x2 then 0 else
    let nb_empty_row = find_interval (fst x1) (fst x2) empty_rows
    and nb_empty_col = find_interval (snd x1) (snd x2) empty_cols in
    let dx = abs((fst x1) - (fst x2))
    and dy = abs((snd x1) - (snd x2)) in
    dx + dy + (expand_ratio - 1) * (nb_empty_row + nb_empty_col)

let for_all_galaxies empty_rows empty_cols expend_ratio galaxies =
    let rec _one_couple g c out =
        match g with
        | e::q ->
            _one_couple q c (out +
                distance_two_galaxies e c
                    empty_cols empty_rows expend_ratio)
        | _ -> out
    in
    let rec _for_all g out =
        match g with
        | e::q -> _for_all q (_one_couple q e out)
        | _ -> out
    in _for_all galaxies 0

let solve f =
    let input, empty_rows, galaxies = get_input f in
    let empty_cols = get_empty_cols input in
    (for_all_galaxies empty_rows empty_cols 2 galaxies),
    (for_all_galaxies empty_rows empty_cols 1000000 galaxies)

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
