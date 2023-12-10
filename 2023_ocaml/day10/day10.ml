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

type point = int * int
module Point = struct
    type t = point
    let compare = compare
end
module PSET = Set.Make(Point)

let is_included str ch =
    match String.index str ch with
    | exception Not_found -> false
    | _ -> true

let print_set s =
    PSET.iter (fun x -> "- " ^ (fst x |> string_of_int) ^ " | - " ^ (snd x |> string_of_int) |> print_endline) s

let find_loop input start_pt =
    let x, y = start_pt in
    let seen = PSET.singleton((x, y)) in
    let queue = Queue.create () in
    Queue.push (x, y) queue;
    let rec _path_finding s =
        if Queue.is_empty queue then s else
        let r, c = Queue.pop queue in
        let ch = input.(r).[c] in
        let ns = (if r > 0 &&
            is_included "S|JL" ch &&
            is_included "|7F" input.(~--r).[c] &&
            not @@ PSET.mem (~--r, c) s
        then let toto = PSET.add (~--r, c) s in Queue.add (~--r, c) queue; toto else s) in
        let ns = (if r + 1 < Array.length input &&
            is_included "S|7F" ch &&
            is_included "|JL" input.(~++r).[c] &&
            not @@ PSET.mem (~++r, c) ns
        then let toto = PSET.add (~++r, c) ns in Queue.add (~++r, c) queue; toto else ns) in
        let ns = (if c > 0 &&
            is_included "S-J7" ch &&
            is_included "-LF" input.(r).[~--c] &&
            not @@ PSET.mem (r, ~--c) ns
        then let toto = PSET.add (r, ~--c) ns in Queue.add (r, ~--c) queue; toto else ns) in
        let ns = (if c + 1 < String.length input.(r) &&
            is_included "S-LF" ch &&
            is_included "-7J" input.(r).[~++c] &&
            not @@ PSET.mem (r, ~++c) ns
        then let toto = PSET.add (r, ~++c) ns in Queue.add (r, ~++c) queue; toto else ns) in
        _path_finding ns
    in let ns = _path_finding seen in
    let len = PSET.cardinal ns in
    ns, len / 2

module CSet = Set.Make(Char)

let potential_start input start_pt =
    let cs = CSet.of_list ['|'; '-'; 'J'; 'L'; '7'; 'F']
    and sr, sc = start_pt in
    let cs = (if sr = 0 ||
        not @@ is_included "|7F" input.(~--sr).[sc]
    then CSet.remove '|' cs |> CSet.remove 'J' |> CSet.remove 'L' else cs) in
    let cs = (if sr = ~--(Array.length input) ||
        not @@ is_included "|JL" input.(~++sr).[sc]
    then CSet.remove '|' cs |> CSet.remove '7' |> CSet.remove 'F' else cs) in
    let cs = (if sc = 0 ||
        not @@ is_included "-LF" input.(sr).[~--sc]
    then CSet.remove '-' cs |> CSet.remove 'J' |> CSet.remove '7' else cs) in
    let cs = (if sc = ~--(String.length input.(sr)) ||
        not @@ is_included "-JF" input.(sr).[~++sc]
    then CSet.remove '-' cs |> CSet.remove 'L' |> CSet.remove 'F' else cs) in
    match CSet.elements cs with
    | e :: [] -> e
    | _ -> failwith "rip :'("

let replace_start input start_pos =
    let ch = potential_start input start_pos in
    input.(fst start_pos) <- (String.mapi (fun i x -> if i = snd start_pos then ch else x) input.(fst start_pos))

let replace_not_loop input pset =
    Array.mapi
        (fun r s ->
            String.mapi
                (fun c ch ->
                    if PSET.mem (r, c) pset then ch else '.')
                s)
        input

let is_within prev is_up ch =
    match ch with
    '|' -> (not prev, false)
    |'L'|'F' -> (prev, ch = 'L')
    |'7'|'J' ->
        let to_cmp = (if is_up then 'J' else '7') in
        if ch <> to_cmp then (not prev, false) else (prev, false)
    | _ -> (prev, is_up)

let get_invalid_set input loop =
    let s = PSET.empty in
    let rec _each_lines x _set =
        if x >= Array.length input then _set else
        let curr = input.(x) in
        let rec _one_line y set within is_up =
            if y >= String.length curr then set else
            let _within, up = is_within within is_up curr.[y] in
            let ns = (if not _within then PSET.add (x, y) set else set) in
            _one_line ~++y ns _within up
        in _one_line 0 _set false false |> _each_lines ~++x
    in let final_set = PSET.union (_each_lines 0 s) loop in
    let len = PSET.cardinal final_set in
    (Array.length input * String.length input.(0)) - len

let solve input start =
    let seen, p1 = find_loop input start in
    replace_start input start;
    let ni = replace_not_loop input seen in
    p1, get_invalid_set ni seen

let () =
    let f = open_in file in
    let input, start_pt = get_input f in
    close_in f;
    let res = solve input start_pt in
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
