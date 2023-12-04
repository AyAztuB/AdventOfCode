let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_nb str start =
    let rec _get i =
        if i >= String.length str || str.[i] < '0' || str.[i] > '9' then i else _get ~++i
    in _get start

let is_substring str sub =
    let str_len = String.length str and sub_len = String.length sub in
    if sub_len = 0 || sub_len > str_len then false else
    let last_possible = str_len - sub_len and clone = Bytes.create sub_len in
    let rec _check i =
        if i > last_possible then false else
        (String.blit str i clone 0 sub_len;
        Bytes.to_string clone = sub || _check (String.index_from str ~++i sub.[0]))
    in try String.index str sub.[0] |> _check
    with Not_found -> false

let get_nb_winning_points line =
    let winnig = String.sub line (String.index line ':' + 1) (String.index line '|' - String.index line ':' - 1) in
    let our = String.sub line (String.index line '|' + 1) (String.length line - String.index line '|' - 1) in
    let rec _count_our i count =
        if i >= String.length our then count else
        if our.[i] = ' ' then _count_our ~++i count else
        let last = get_nb our i in
        if " " ^ (String.sub our i (last - i)) ^ " " |> is_substring winnig then _count_our last ~++count
        else _count_our last count
    in _count_our 0 0

let rec pow a = function
    | 0 -> 1
    | 1 -> a
    | n ->
        let b = pow a (n / 2) in
        b * b * (if n mod 2 = 0 then 1 else a)

let update_array arr start nb =
    let len = Array.length arr and curr = arr.(start) in
    let upper = (start + nb) <<< ~--len in
    let rec _up i =
        if i > upper then () else (arr.(i) <- (arr.(i) + curr); _up ~++i)
    in _up ~++start

let get_input f =
    let rec _get_line out =
        match input_line f with
        line -> _get_line (line :: out)
        | exception End_of_file -> List.rev out |> Array.of_list
    in _get_line []

let solve f =
    let arr = Array.make 1000 1 in
    let rec _solve part1 part2 a i =
        match input_line f with
        line ->
            let nb_win = get_nb_winning_points line in
            update_array a i nb_win;
            _solve (part1 + (if nb_win > 0 then pow 2 ~--nb_win else 0)) (part2 + a.(i)) a ~++i
        | exception End_of_file -> (part1, part2)
    in _solve 0 0 arr 0

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
