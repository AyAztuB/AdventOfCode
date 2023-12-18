let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let dirs = function
    | 'U' | '3' -> (-1, 0)
    | 'D' | '1' -> (1, 0)
    | 'R' | '0' -> (0, 1)
    | 'L' | '2' -> (0, -1)
    | _ -> failwith "bad input"

let new_point last_point line =
    match String.split_on_char ' ' line with
    | dir::n::_::[] ->
        let dr, dc = dirs dir.[0] and nn = int_of_string n in
        (nn, ((fst last_point) + dr * nn, (snd last_point) + dc * nn))
    | _ -> failwith "bad input"

let new_pt_p2 last_point line =
    match String.split_on_char ' ' line with
    | _::_::color::[] ->
        let col = String.sub color 2 (String.length color - 3) in
        let dr, dc = dirs col.[String.length col - 1]
        and nn = int_of_string ("0x" ^ (String.sub col 0 (String.length col - 1))) in
        (nn, ((fst last_point) + dr * nn, (snd last_point) + dc * nn))
    | _ -> failwith "bad input"

let shoelace_formula points =
    let arr = Array.of_list points in
    let rec _sum i out =
        if i >= Array.length arr then out else
        let curr = ((fst arr.(i)) * (snd arr.((i+1) mod Array.length arr) - snd arr.(if i-1 >= 0 then i-1 else Array.length arr -1))) in
        _sum ~++i (out + curr)
    in let s = (_sum 0 0) / 2 in
    if s < 0 then -s else s

let picks_theorem area nb_points =
    area + 1 - nb_points / 2

let solve f =
    let points = ref []
    and pt2 = ref [] in
    let rec _solve nb_points nb_pt2 last_point last_p2 =
        match input_line f with
        line ->
            let n, ((r, c) as point) = new_point last_point line
            and n2, ((r2, c2) as p2) = new_pt_p2 last_p2 line in
            points := point::!points;
            pt2 := p2::!pt2;
            _solve (n + nb_points) (n2 + nb_pt2) point p2
        | exception End_of_file ->
            (nb_points + picks_theorem (shoelace_formula !points) nb_points),
            (nb_pt2 + picks_theorem (shoelace_formula !pt2) nb_pt2)
    in _solve 0 0 (0, 0) (0, 0)

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
