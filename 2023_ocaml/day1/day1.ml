let file = "input"

let is_digit str i step =
    match str.[i] with '0'..'9' -> int_of_char str.[i] - int_of_char '0' | _ -> -1

let list_of_num = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

let is_sub str i step num =
    let start = match step with -1 -> (String.length num) -1 | _ -> 0 in
    let rec _is_eq k_str k_num =
        if k_num >= String.length num || k_num < 0 then true
        else if  k_str < 0 || k_str >= String.length str then false
        else
        if str.[k_str] = num.[k_num] then _is_eq (k_str + step) (k_num + step)
        else false
    in _is_eq i start

let is_digit_with_str str i step =
    let basic = is_digit str i step in
    if basic >= 0 then basic
    else
    let rec _check nums res =
        match nums with
        e::q -> if is_sub str i step e then res + 1
                else _check q (res + 1)
        | _-> -1
    in _check list_of_num 0

let get_digit str start step is_digit_fun =
    (* assuming there is at least one digit => no need to check if 'i' overflow *)
    let rec _get_digit i =
        let n = is_digit_fun str i step in
        if n >= 0 then n
        else _get_digit (i + step)
    in _get_digit start

let get_number str is_digit_fun =
    let first = get_digit str 0 1 is_digit_fun in
    let last = get_digit str ((String.length str) - 1) (-1) is_digit_fun in
    first * 10 + last

let solve f =
    let rec _solve part1 part2 =
        match input_line f with
        line -> _solve (part1 + get_number line is_digit) (part2 + get_number line is_digit_with_str)
        | exception End_of_file -> (part1, part2)
    in _solve 0 0

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    print_endline (Format.sprintf "part1: %d" (fst res));
    print_endline (Format.sprintf "part2: %d" (snd res))
