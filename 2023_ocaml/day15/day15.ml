let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let hash str =
    let rec _hash_by_char idx res =
        if idx >= String.length str then res else
        _hash_by_char ~++idx
            (((res + int_of_char str.[idx]) * 17) mod 256)
    in _hash_by_char 0 0

let part1 input =
    let rec _aux res = function
        | [] -> res
        | e::q -> _aux (res + hash e) q
    in _aux 0 input

let part2 input =
    let boxes = ref (Array.make 256 [])
    and store = ref (Hashtbl.create 500) in
    let _add_label idx label =
        if not @@ List.exists (fun s -> s = label) !boxes.(idx)
        then !boxes.(idx) <- !boxes.(idx) @ [label]
    in
    let rec _for_all current =
        match current with
        | [] -> ()
        | e::q ->
            if e.[String.length e -1] = '-'
            then (
                let label = String.sub e 0 (String.length e -1) in
                let index = hash label in
                !boxes.(index) <- List.filter (fun x -> x <> label) !boxes.(index);
                _for_all q
            )
            else (
                match String.split_on_char '=' e with
                | label::length::[] ->
                    let l = int_of_string length
                    and idx = hash label in
                    Hashtbl.add !store label l;
                    _add_label idx label;
                    _for_all q
                | _ -> failwith "bad input"
            )
    in
    let rec _eval idx res =
        if idx >= Array.length !boxes then res else
        let rec _eval_box l i r =
            match l with
            [] -> r
            | e::q -> _eval_box q ~++i
                (r + ~++idx * ~++i * Hashtbl.find !store e)
        in _eval ~++idx (res + _eval_box !boxes.(idx) 0 0)
    in _for_all input; _eval 0 0

let solve f =
    let input = input_line f |> String.split_on_char ',' in
    part1 input, part2 input

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
