let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let is_digit ch = match ch with '0'..'9' -> true | _ -> false

let get_seeds input =
    match String.split_on_char ':' input with
    | _ :: nbs :: _ ->
        let nb = nbs |> String.split_on_char ' ' in
        let rec _map_nb seeds out =
            match seeds with
            | e::q when String.length e > 0 -> _map_nb q ((int_of_string e)::out)
            | _::q -> _map_nb q out
            | _ -> out
        in _map_nb nb []
    | _ -> failwith "bad parsing"

let map_seeds_ranges seeds =
    let rec _map s out =
        match s with
        | e1::e2::q -> _map q ((e1, e1 + e2)::out)
        | _ -> out
    in _map (List.rev seeds) []

let convert_seeds out seeds line =
    if String.length line = 0 then
        let rec _complete s o =
        match s with
        | e::q -> _complete q (e::o)
        | _ -> ([], o)
    in _complete seeds out else
    if not @@ is_digit line.[0] then (out, seeds) else
    let l = String.split_on_char ' ' line |> Array.of_list |> Array.map int_of_string in
    let rec _match_seeds s o =
        match s with
        | e::q when e >= l.(1) && e < l.(1) + l.(2) -> _match_seeds q ((e - l.(1) + l.(0))::o)
        | e::q -> let res, nseed = _match_seeds q o in (res, e::nseed)
        | _ -> (o, [])
    in _match_seeds seeds out

let convert_seeds_ranges out seeds line =
    if String.length line = 0 then
    let rec _complete s o =
        match s with
        | e::q -> _complete q (e::o)
        | _ -> ([], o)
    in _complete seeds out else
    if not @@ is_digit line.[0] then (out, seeds) else
    let l = String.split_on_char ' ' line |> Array.of_list |> Array.map int_of_string in
    let rec _match_seeds s o =
        match s with
        | e::q ->
            let start = (fst e) >>> l.(1) and last = (snd e) <<< (l.(1) + l.(2)) in
            let _is_sub = start < last in
            if _is_sub then
                (
                let res, nseed = _match_seeds q ((start - l.(1) + l.(0), last - l.(1) + l.(0))::o) in
                let ns = (if start > fst e then (fst e, start)::nseed else nseed) in
                let ns2 = (if snd e > last then (last, snd e)::ns else ns) in
                (res, ns2)
            )
            else
                let res, nseed = _match_seeds q o in (res, e::nseed)
        | _ -> (o, [])
    in _match_seeds seeds out

let output_p1 out seeds =
    let rec _full_out s o =
        match s with
        | e::q -> _full_out q (e::o)
        | _ -> o
    in let output = _full_out seeds out in
    let rec _min o min = 
        match o with
        | e::q when e < min -> _min q e
        | _::q -> _min q min
        | _ -> min
    in _min output Int.max_int

let output_p2 out seeds =
    let rec _full_out s o =
        match s with
        | e::q -> _full_out q (e::o)
        | _ -> o
    in let output = _full_out seeds out in
    let rec _min o min =
        match o with
        | e::q when fst e < min -> _min q (fst e)
        | _::q -> _min q min
        | _ -> min
    in _min output Int.max_int

let get_input f =
    let rec _get_line out =
        match input_line f with
        line -> _get_line (line :: out)
        | exception End_of_file -> List.rev out |> Array.of_list
    in _get_line []

let solve f =
    let seeds = get_seeds @@ input_line f in
    let rec _solve seed_p1 seed_p2 out_p1 out_p2 =
        match input_line f with
        line ->
            let out1, seed1 = convert_seeds out_p1 seed_p1 line in
            let out2, seed2 = convert_seeds_ranges out_p2 seed_p2 line in
            _solve seed1 seed2 out1 out2
        | exception End_of_file -> (output_p1 out_p1 seed_p1, output_p2 out_p2 seed_p2)
    in _solve seeds (map_seeds_ranges seeds) [] []

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
