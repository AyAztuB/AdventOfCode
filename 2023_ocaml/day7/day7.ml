let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let map_line line =
    match String.split_on_char ' ' line with
    | cards::value::[] -> (cards, int_of_string value)
    | _ -> failwith "bad input"

let get_input f =
    let rec _get_line out =
        match input_line f with
        line -> _get_line ((map_line line):: out)
        | exception End_of_file -> List.rev out |> Array.of_list
    in _get_line []

let is_in_array x arr =
    Array.exists (fun c -> c = x) arr

let get_nb_two arr =
    let rec _get idx res =
        if idx >= Array.length arr then res else
        if arr.(idx) = 2 then _get ~++idx ~++res else _get ~++idx res
    in _get 0 0

let score cards nb_jockers =
    let counts = String.fold_left
        (fun x c -> x.(int_of_char c) <- x.(int_of_char c) + 1; x)
        (Array.make 256 0)
        cards in
    if is_in_array 5 counts then 6 else
    if is_in_array 4 counts then (
        6 <<< (5 + nb_jockers)
    ) else
    if is_in_array 3 counts then (
        if is_in_array 2 counts then (
            6 <<< (4 + nb_jockers)
        ) else (
            5 <<< (3 + nb_jockers * 2)
        )
    ) else
    if is_in_array 2 counts then (
        if 2 = get_nb_two counts then (
            if nb_jockers = 2 then 5 else 2 + nb_jockers * 2
        ) else (
            if nb_jockers > 0 then 3 else 1
        )
    ) else nb_jockers

let is_existing x letters =
    let rec _is i =
        if i >= Array.length letters then -1 else
        if fst letters.(i) = x then i else _is ~++i
    in _is 0

let letters_p1 = [|('T', 'A'); ('J', 'B'); ('Q', 'C'); ('K', 'D'); ('A', 'E')|]
let letters_p2 = [|('T', 'A'); ('J', '.'); ('Q', 'C'); ('K', 'D'); ('A', 'E')|]

let replace cards letters =
    String.map
        (fun x ->
            let i = is_existing x letters in
            if i = -1 then x else snd letters.(i))
        cards

let count_jockers cards =
    let rec _count i res =
        if i >= String.length cards then res else
        if cards.[i] = 'J' then _count ~++i ~++res else _count ~++i res
    in _count 0 0

let get_score_p1 x =
    (score (fst x) 0, replace (fst x) letters_p1)

let get_score_p2 x =
    (score (fst x) (count_jockers @@ fst x), replace (fst x) letters_p2)

let solver input score_fun =
    Array.sort
        (fun x y ->
            compare (score_fun x) (score_fun y))
        input;
    let rec _eval i res arr =
        if i >= Array.length arr then res
        else _eval ~++i (res + (~++i) * (snd arr.(i))) arr
    in _eval 0 0 input

let solve input = solver input get_score_p1, solver input get_score_p2

let () =
    let f = open_in file in
    let input = get_input f in
    close_in f;
    let res = solve input in
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
