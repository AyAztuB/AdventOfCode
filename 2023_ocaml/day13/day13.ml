let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let is_mirror idx len m_compare part =
    let rec _is_mirror curr p =
        if curr >= len || curr - idx + 1 > idx then p = 0 else
        let mirror_idx = idx - (curr-idx+1) in
        let cmp_res = m_compare mirror_idx curr in
        if cmp_res <= p
        then _is_mirror ~++curr (p - cmp_res)
        else false
    in _is_mirror idx part

let get_mirror len m_compare part =
    let rec _get_mirror idx =
        if idx >= len then 0 else
        if is_mirror idx len m_compare part then idx else
        _get_mirror ~++idx
    in _get_mirror 1

let part1_mirrors grid =
    let row = get_mirror (Array.length grid)
        (fun x y -> if compare grid.(x) grid.(y) = 0 then 0 else 1) 0
    and col = get_mirror (String.length grid.(0))
        (fun x y -> if Array.for_all (fun s -> s.[x] = s.[y]) grid then 0 else 1) 0
    in
    row * 100 + col

let part2_mirrors grid =
    let inner_cmp idx error len cmp_fun = 
        let rec _inner i err =
            if err > 1 || i >= len then err else
            if cmp_fun i then _inner ~++i err else
            _inner ~++i ~++err
        in _inner idx error
    in
    let _cmp_2_rows x y =
        let l1 = grid.(x) and l2 = grid.(y) in
        let len = String.length l1 in
        inner_cmp 0 0 len (fun t -> l1.[t] = l2.[t])
    in
    let _cmp_2_cols x y =
        let len = Array.length grid in
        inner_cmp 0 0 len (fun t -> grid.(t).[x] = grid.(t).[y])
    in
    let row = get_mirror (Array.length grid) _cmp_2_rows 1
    and col = get_mirror (String.length grid.(0)) _cmp_2_cols 1
    in
    row * 100 + col

let get_input f =
    let rec _get_line out =
        match input_line f with
        | "" | exception End_of_file -> List.rev out |> Array.of_list
        | line -> _get_line (line :: out)
    in _get_line []

let solve f =
    let rec _solve part1 part2 =
        match get_input f with
        | [||] -> (part1, part2)
        | x -> _solve (part1 + part1_mirrors x) (part2 + part2_mirrors x)
    in _solve 0 0

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
