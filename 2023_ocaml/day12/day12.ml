let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let parse_line line =
    match String.split_on_char ' ' line with
    | config::nums::[] -> (
        config,
        (String.split_on_char ',' nums |> List.map int_of_string)
    )
    | _ -> failwith "bad input"

let transforme_step_2 config nums =
    let rec _reconstitute str num idx =
        if idx = 0 then (str, List.concat num) else
        _reconstitute (str ^ "?" ^ config) (nums::num) ~--idx
    in _reconstitute config [nums] (5-1)

let is_in_cache tuple cache f =
    match Hashtbl.find cache tuple with
    | x -> x
    | exception Not_found ->
        let res = f tuple in
        Hashtbl.add cache tuple res; res

let count_arrangements spring nums cache =
    let rec _count _conf _num =
        let _inner (conf, num) =
            match conf, num with
            | "", [] -> 1 (* valid sub sequence *)
            | "", _ -> 0 (* invalid sub sequence *)
            | s, [] when
                String.for_all (fun c -> c <> '#') s -> 1 (* valid sub sequence *)
            | _, [] -> 0 (* invalid sub sequence *)
            | s, e::q ->
                let result = (
                    if s.[0] = '.' || s.[0] = '?'
                    then _count (String.sub s 1 (String.length s - 1)) (e::q)
                    else 0) in
                let result = (
                    if (s.[0] = '#' || s.[0] = '?')
                        && e <= String.length s
                        && String.for_all (fun c -> c <> '.') (String.sub s 0 e)
                        && (e = String.length s || s.[e] <> '#')
                    then (
                        if e = String.length s then _count "" q else
                        _count (String.sub s (e + 1) (String.length s - e - 1)) q
                    ) + result
                    else result
                ) in result
        in is_in_cache (_conf, _num) cache _inner
    in _count spring nums

let solve f =
    let cache = Hashtbl.create 1000000 in
    let rec _solve part1 part2 =
        match input_line f with
        line ->
            let config, nums = parse_line line in
            let conf2, num2 = transforme_step_2 config nums in
            let curr1 = count_arrangements config nums cache
            and curr2 = count_arrangements conf2 num2 cache in
            _solve (part1 + curr1) (part2 + curr2)
        | exception End_of_file -> (part1, part2)
    in _solve 0 0

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
