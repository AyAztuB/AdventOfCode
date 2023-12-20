let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let match_indexes = function
    | 'x' -> 0
    | 'm' -> 1
    | 'a' -> 2
    | 's' -> 3
    | _ -> failwith "bad ressource"

let ope = function
    | '>' -> (>)
    | '<' -> (<)
    | _ -> failwith "bad operator"

let get_input f =
    let rec _parse_workflow out =
        match input_line f with
        | "" | exception End_of_file -> out
        | line -> (
            match String.split_on_char '{' (String.sub line 0 (String.length line -1)) with
            | name :: block :: [] -> (
                let rules = String.split_on_char ',' block in
                let rec _aux r o =
                    match r with
                    | [] -> failwith "should never happen"
                    | [default] -> (List.rev o, default)
                    | r::q -> (
                            match String.split_on_char ':' r with
                            | comp :: target :: [] ->
                                let key = comp.[0]
                                and cmp = comp.[1]
                                and n = String.sub comp 2 (String.length comp -2) |> int_of_string
                                in _aux q ((key, cmp, n, target)::o)
                            | _ -> failwith "bad input sub workflow"
                        )
                in let r = (_aux rules []) in
                Hashtbl.add out name r;
                _parse_workflow out
            )
            | _ -> failwith "bad input workflows"
        )
    in
    let rec _parse_blocks out =
        match input_line f with
        | exception End_of_file -> out
        | line -> (
                let rec _aux l o =
                    match l with
                    | [] -> o
                    | e::q -> (
                            match String.split_on_char '=' e with
                            | ch::n::[] ->
                                o.(match_indexes ch.[0]) <- (int_of_string n);
                                _aux q o
                            | _ -> failwith ("bad input sub block: get " ^ e)
                        )
                in _parse_blocks ((_aux (String.split_on_char ',' (String.sub line 1 (String.length line - 2))) (Array.make 4 0))::out)
            )
    in let w = _parse_workflow (Hashtbl.create 500)
    in w, _parse_blocks []

let accept item workflow =
    let rec _is_accepted rule =
        match rule with
        | [] -> ""
        | (key, cmp, n, target)::q ->
            if (ope cmp) item.(match_indexes key) n then target else
            _is_accepted q
    in
    let rec _aux rule_name =
        match rule_name with
        | "R" -> false
        | "A" -> true
        | r ->
            let rule, default = Hashtbl.find workflow r in
            let nr = _is_accepted rule in
            if nr = "" then _aux default else _aux nr
    in _aux "in"

let part1 workflows ratings =
    let rec _for_all items out =
        match items with
        | [] -> out
        | e::q ->
            if accept e workflows
            then _for_all q (Array.fold_left (fun a b -> a + b) out e)
            else _for_all q out
    in _for_all ratings 0

let part2 workflows =
    let rec _count ranges rule_name =
        match rule_name with
        | "R" -> 0
        | "A" -> Array.fold_left (fun acc (lo, hi) -> acc * (hi - lo + 1)) 1 ranges
        | r -> (
                let rules, default = Hashtbl.find workflows r in
                let rec _each_rule r curr_ranges =
                    match r with
                    | [] -> _count curr_ranges default
                    | (key, cmp, n, target)::q ->
                        let lo, hi = curr_ranges.(match_indexes key) in
                        let ok, bad = (if cmp = '<'
                            then ((lo, (n-1) <<< hi), (n >>> lo, hi))
                            else (((n+1) >>> lo, hi), (lo, n <<< hi))) in
                        let out = (if fst ok <= snd ok
                            then _count (Array.mapi (fun i x -> if i = match_indexes key then ok else x) curr_ranges) target
                            else 0) in
                        if fst bad <= snd bad
                        then out + _each_rule q (Array.mapi (fun i x -> if i = match_indexes key then bad else x) curr_ranges)
                        else out
                in _each_rule rules ranges
            )
    in _count (Array.make 4 (1, 4000)) "in"

let solve workflows ratings =
    part1 workflows ratings,
    part2 workflows

let () =
    let f = open_in file in
    let workflows, ratings = get_input f in
    close_in f;
    let res = solve workflows ratings in
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
