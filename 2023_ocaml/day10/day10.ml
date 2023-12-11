let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_input f =
    let start_line = ref (-1) and start_in = ref (-1) in
    let rec _get_line out l =
        match input_line f with
        line -> (
                match String.index line 'S' with
                | x -> start_in := x; start_line := l
                | exception Not_found -> ()
            );
            _get_line (line :: out) ~++l
        | exception End_of_file -> List.rev out |> Array.of_list
    in let input = (_get_line [] 0) in
    (input, (!start_line, !start_in))

let up = (-1, 0)
let down = (1, 0)
let right = (0, 1)
let left = (0, -1)

let dirs = function
    | '|' -> (up, down)
    | '-' -> (left, right)
    | 'F' -> (down, right)
    | 'J' -> (up, left)
    | 'L' -> (up, right)
    | '7' -> (down, left)
    | _ -> failwith "never reached"

let is_included str ch =
    match String.index str ch with
    | exception Not_found -> false
    | _ -> true

let find_path input start =
    let rec _iter prev curr out =
        let s1, s2 = dirs input.(fst curr).[snd curr] in
        let next = (if (fst curr + fst s1, snd curr + snd s1) = prev
            then (fst curr + fst s2, snd curr + snd s2)
            else (fst curr + fst s1, snd curr + snd s1)) in
        if next = start then (curr::out) else
        _iter curr next (curr :: out)
    in _iter (-1, -1) start []

let shoelace_formula seen =
    let arr = Array.of_list seen in
    let rec _sum i out =
        if i >= Array.length arr then out else
        let curr = ((fst arr.(i)) * (snd arr.((i+1) mod Array.length arr) - snd arr.(if i-1 >= 0 then i-1 else Array.length arr -1))) in
        _sum ~++i (out + curr)
    in let s = (_sum 0 0) / 2 in
    if s < 0 then -s else s

let picks_theorem area seen_count =
    area + 1 - seen_count / 2

let part2 seen =
    (* Use the Shoelace Formula to get the area of the polygon *)
    let area = shoelace_formula seen in
    (* Then we use the Pick's Theorem to get the number of nodes the polygon contains fully within *)
    picks_theorem area (List.length seen)

module CSet = Set.Make(Char)

let potential_start input start_pt =
    let cs = CSet.of_list ['|'; '-'; 'J'; 'L'; '7'; 'F']
    and sr, sc = start_pt in
    let cs = (if sr = 0 ||
        not @@ is_included "|7F" input.(~--sr).[sc]
    then CSet.diff cs (CSet.of_list ['|'; 'J'; 'L']) else cs) in
    let cs = (if sr = ~--(Array.length input) ||
        not @@ is_included "|JL" input.(~++sr).[sc]
    then CSet.diff cs (CSet.of_list ['|'; '7'; 'F']) else cs) in
    let cs = (if sc = 0 ||
        not @@ is_included "-LF" input.(sr).[~--sc]
    then CSet.diff cs (CSet.of_list ['-'; '7'; 'J']) else cs) in
    let cs = (if sc = ~--(String.length input.(sr)) ||
        not @@ is_included "-JF" input.(sr).[~++sc]
    then CSet.diff cs (CSet.of_list ['-'; 'L'; 'F']) else cs) in
    match CSet.elements cs with
    | e :: [] -> e
    | _ -> failwith "rip :'("

let replace_start input start_pos =
    let ch = potential_start input start_pos in
    input.(fst start_pos) <- (String.mapi (fun i x -> if i = snd start_pos then ch else x) input.(fst start_pos))

let solve input start =
    replace_start input start;
    let seen = find_path input start in
    (List.length seen) / 2, part2 seen

let () =
    let f = open_in file in
    let input, start_pt = get_input f in
    close_in f;
    let res = solve input start_pt in
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
