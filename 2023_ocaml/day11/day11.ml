let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y
let (?-) x = if x >= 0 then x else -x

let get_input f =
    let rec _get_line out =
        match input_line f with
        line ->
            _get_line (line :: out)
        | exception End_of_file ->
            List.rev out |> Array.of_list
    in _get_line []

let construct_psa input len scale is_empty_fun =
    let psa = ref (Array.make (len + 1) 0) in
    let rec _iter i =
        if i >= len then !psa else
        (!psa.(i) <- (!psa.(i - 1) + (if is_empty_fun input i then scale else 1));
        _iter ~++i)
    in _iter 1

let is_row_empty input i =
    String.for_all (fun ch -> ch = '.') input.(i)

let is_col_empty input i =
    Array.for_all (fun s -> s.[i] = '.') input

let count_galaxies input len fold_left_fun =
    let rec _get i out =
        if i >= len then out else
        (
            let c = fold_left_fun input i in
            if c > 0 then _get ~++i ((i, c)::out) else
            _get ~++i out
        )
    in _get 0 []

let fold_left_row input i =
    String.fold_left (fun acc ch -> if ch = '#' then acc + 1 else acc) 0 input.(i)

let fold_left_col input i =
    Array.fold_left (fun acc s -> if s.[i] = '#' then acc + 1 else acc) 0 input

let compute galaxies psa =
    let rec _aux g nb_seen _sum res =
        match g with
        | (idx, count)::q ->
            _aux q (nb_seen + count) (_sum + psa.(idx) * count)
                (res + (nb_seen * psa.(idx) - _sum) * count)
        | _ -> res
    in _aux galaxies 0 0 0

let solve input scale galaxies =
    let row_psa = construct_psa input (Array.length input) scale is_row_empty
    and col_psa = construct_psa input (String.length input.(0)) scale is_col_empty
    and g_rows, g_cols = galaxies in
    (* because galaxies are in the inverse order, the result will be negative... *)
    ?-((compute g_rows row_psa) + (compute g_cols col_psa))

let () =
    let f = open_in file in
    let input= get_input f in
    close_in f;
    let galaxies_rows = count_galaxies input (Array.length input) fold_left_row
    and galaxies_cols = count_galaxies input (String.length input.(0)) fold_left_col in
    let part1 = solve input 2 (galaxies_rows, galaxies_cols)
    and part2 = solve input 1000000 (galaxies_rows, galaxies_cols) in
    "part1: " ^ (part1 |> string_of_int) ^ "\npart2: " ^ (part2 |> string_of_int)
    |> print_endline
