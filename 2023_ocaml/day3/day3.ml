let file = "input"

let (~++) x = x+1
let (~--) x = x-1

let check_num l start last above_sym =
    if last < String.length l && l.[last] <> '.' then true else
    if start > 0 && l.[start-1] <> '.' then true else
    let rec _check_above asym =
        match asym with
        | e :: q -> if e >= start - 1 && e <= last then true else _check_above q
        | _ -> false
    in _check_above above_sym

let check_sym i above_num p1 =
    let rec _check_sym anum sum out_nanum =
        match anum with
        | (n, (s, e)) :: q ->
            if i >= (s-1) && i <= e
                then _check_sym q (n+sum) out_nanum
            else _check_sym q sum ((n, (s, e))::out_nanum)
        | _ -> (sum, out_nanum)
    in _check_sym above_num p1 []

let new_gear i anum base =
    let rec _create nums r =
        match nums with
        | (n, (s, e))::q ->
            if i >= (s-1) && i <= e
                then _create q (((fst @@ fst r)+1, i), ((if fst @@ fst r >= 1 then fst @@ snd r else n), (if fst @@ fst r == 1 then n else snd @@ snd r)))
            else _create q r
        | _ -> r
    in _create anum base

(* gears = current gears *)
let check_new_gear gears i anum line =
    let rec _get_prev_num k =
        if k < 0 then 0 else
        match line.[k] with
        | '0'..'9' -> _get_prev_num ~--k
        | _ -> ~++k
    in let _s_num = _get_prev_num ~--i in
    let p_num = (if _s_num < i then String.sub line _s_num (i - _s_num) |> int_of_string else 0) in
    let rec _get_next_num k =
        if k >= String.length line then k else
        match line.[k] with
        | '0'..'9' -> _get_next_num ~++k
        | _ -> k
    in let _e_num = _get_next_num ~++i in
    let n_num = (if _e_num <> i+1 then String.sub line ~++i (_e_num - i - 1) |> int_of_string else 0) in
    let _g = (((if _s_num < i then 1 else 0), i), (p_num, 0)) in
    let _g_2 = (((if _e_num <> i+1 then (fst @@ fst _g) + 1 else fst @@ fst _g), i), ((if fst @@ fst _g = 1 then p_num else n_num), (if fst @@ fst _g = 1 then n_num else 0))) in
    let g = new_gear i anum  _g_2 in
    if fst @@ fst g <= 2 then g::gears else gears

(* gears = above gears *)
let check_num_gear gears i last num =
    let rec _change_prev g out =
        match g with
        | ((nb, idx), (n1, n2)) :: q ->
            if idx >= i - 1 && idx <= last then (
                    if nb = 2 then _change_prev q out
                    else _change_prev q (((~++nb, idx), ((if nb = 1 then n1 else num), (if nb = 1 then num else 0)))::out))
            else _change_prev q (((nb, idx), (n1, n2))::out)
        | _ -> out
    in _change_prev gears []

let compute_gear gears =
    let rec _compute g r =
        match g with
        | ((nb, _), (n1, n2))::q -> if nb = 2 then _compute q (r + n1*n2) else _compute q r
        | _ -> r
    in _compute gears 0

let solve_line line above_num above_sym gears =
    let rec _iter_line i anum p1 out_unknown out_sym g ag =
        if i >= String.length line then (p1, out_unknown, out_sym, g, compute_gear ag) else
        if line.[i] = '.' then _iter_line ~++i anum p1 out_unknown out_sym g ag
        else match line.[i] with
        | '0'..'9' ->
            let rec _get_last_digit_index k =
                if k >= String.length line then k else
                match line.[k] with
                | '0'..'9' -> _get_last_digit_index ~++k
                | _ -> k
            in let last = _get_last_digit_index i in
            let num = String.sub line i (last - i) |> int_of_string in
            if check_num line i last above_sym
                then _iter_line last anum (num + p1) out_unknown out_sym g (check_num_gear ag i last num)
            else _iter_line last anum p1 ((num, (i, last)) :: out_unknown) out_sym g (check_num_gear ag i last num)
        | x ->
            let (_p1, nanum) = check_sym i anum p1 in
            _iter_line ~++i nanum _p1 out_unknown (i::out_sym) (if x = '*' then check_new_gear g i above_num line else g) ag
    in _iter_line 0 above_num 0 [] [] [] gears

let sum_list l =
    let rec _sum list out =
        match list with
        | e::q -> _sum q (out + e)
        | _ -> out
    in _sum l 0

let solve f =
    let rec _solve p1 p2 above_nums above_syms above_gear =
        match input_line f with
        line ->
            let (nums, anum, asym, gears, sg) = solve_line line above_nums above_syms above_gear in
            _solve (p1 + nums) (p2 + sg) anum asym gears
        | exception End_of_file -> p1, p2
    in _solve 0 0 [] [] []

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    ("part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int))
    |> print_endline
