let file = "input"

let is_digit ch =
    match ch with '0'..'9' -> true | _ -> false

let get_int ch = int_of_char ch - int_of_char '0'

let get_num str i =
    let rec _get k r =
        if is_digit str.[k] then _get (k + 1) (r * 10 + get_int str.[k])
        else (k, r)
    in _get i 0

let game_id str = get_num str 5

let colors = ["red"; "green"; "blue"]
let colors_len = [| 3; 5; 4 |]

let skip_whitespaces str i =
    let rec _skip k =
        match str.[k] with
        ' '|':'|','|';' -> _skip (k + 1)
        | _ -> k
    in _skip i

let get_color str i =
    let i2 = skip_whitespaces str i in
    let (i3, nb) = get_num str i2 in
    let i4 = skip_whitespaces str i3 in
    let rec _eq num _i _k =
        if _k >= String.length num then true
        else if _i >= String.length str || str.[_i] <> num.[_k] then false
        else _eq num (_i + 1) (_k + 1)
    in let rec _witch cols r =
        match cols with
        e::q -> if _eq e i4 0 then r else _witch q (r + 1)
        | _ -> -1
    in let _color = _witch colors 0 in
    (i4 + colors_len.(_color)), nb, _color

let possible str =
    let max_per_colors = [| 0; 0; 0 |] in
    let i, id = game_id str in
    let rec _one_chunk k =
        let index, nb, color = get_color str k in
            (if nb > max_per_colors.(color) then max_per_colors.(color) <- nb);
            (index, nb <= (12 +color))
    in let rec _all k isposs =
        if k >= ((String.length str) - 2) then isposs
        else let _i, _poss = _one_chunk k in
        _all _i (isposs && _poss)
    in let isposs = _all i true in
    let power = max_per_colors.(0) * max_per_colors.(1) * max_per_colors.(2) in
    if isposs then (id, power)
    else (0, power)

let solve f =
    let rec _solve part1 part2 =
        match input_line f with
        line ->
            let _r1, _r2 = possible line in
            _solve (part1 + _r1) (part2 + _r2)
        | exception End_of_file -> (part1, part2)
    in _solve 0 0

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    print_endline (Format.sprintf "part1: %d\npart2: %d" (fst res) (snd res))
