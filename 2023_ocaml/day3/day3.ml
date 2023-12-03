let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let is_num ch = match ch with '0'..'9' -> true | _ -> false
let is_dot ch = ch = '.'
let is_sym ch = not @@ is_num ch && not @@ is_dot ch

let get_num str start last =
    last - start |> String.sub str start |> int_of_string

let parse_num str start =
    let rec _parse i =
        if i >= String.length str then i else
        if is_num str.[i] then _parse ~++i else i
    in _parse start

let get_num_start str i =
    let rec _get k =
        if k < 0 || not @@ is_num str.[k] then ~++k else _get ~--k
    in _get i

let check_sym_bellow_or_above str start last =
    let rec _check i =
        if i < 0 then _check ~++i else
        if i >= String.length str || i > last then false else
        if is_sym str.[i] then true else _check ~++i
    in _check ~--start

let is_valid_num arr i start last =
    (i > 0 && check_sym_bellow_or_above arr.(~--i) start last)
    || (i < ~--(Array.length arr) && check_sym_bellow_or_above arr.(~++i) start last)
    || (start > 0 && is_sym arr.(i).[~--start])
    || (last < String.length arr.(i) && is_sym arr.(i).[last])

let check_for_nums arr i k =
    let rec _check str j out =
        if j > ~++k || j >= String.length str then out else
        if j < 0 then _check str ~++j out else
        if is_num str.[j] then (
        let start = get_num_start str j in
        let last = parse_num str start in
        let num = get_num str start last in
        let nout = ((fst out |> (~++)), (num * snd out)) in
        _check str last nout
        ) else _check str ~++j out
    in if i >= 0 && i < Array.length arr then _check arr.(i) ~--k (0, 1) else (0, 1)

let get_gear_value arr i k =
    let above = check_for_nums arr ~--i k in
    let bellow = check_for_nums arr ~++i k in
    let current = check_for_nums arr i k in
    if (fst current) + (fst above) + (fst bellow) <> 2 then 0
    else (snd current) * (snd above) * (snd bellow)

let solve_line arr i =
    let rec _solve str k p1 p2 =
        if k >= String.length str then (p1, p2) else
        match str.[k] with
        | '0'..'9' ->
            let last = parse_num str k in
            let num = get_num str k last in
            if is_valid_num arr i k last then _solve str last (p1 + num) p2
            else _solve str last p1 p2
        | '*' -> _solve str ~++k p1 (p2 + get_gear_value arr i k)
        | _ -> _solve str ~++k p1 p2
    in _solve arr.(i) 0 0 0

let get_input f =
    let rec _get_line out =
        match input_line f with
        | line -> _get_line (("." ^ line ^ ".") :: out)
        | exception End_of_file -> Array.of_list out
    in _get_line []

let solve input =
    let rec _solve i p1 p2 =
        if i >= Array.length input then (p1, p2)
        else let _res_line = solve_line input i in
        _solve ~++i (p1 + fst _res_line) (p2 + snd _res_line)
    in _solve 0 0 0

let () =
    let f = open_in file in
    let input = get_input f in
    close_in f;
    let res = solve input in
    ("part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int))
    |> print_endline
