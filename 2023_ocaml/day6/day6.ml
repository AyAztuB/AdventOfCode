let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_list_of_int line =
    match String.split_on_char ':' line with
    | _ :: nbs :: _ ->
        String.split_on_char ' ' nbs
        |> List.filter (fun x -> String.length x > 0)
        |> List.map (int_of_string)
    | _ -> failwith "bad input"

let get_one_int line =
    match String.split_on_char ':' line with
    | _ :: nbs :: _ ->
        String.split_on_char ' ' nbs
        |> List.filter (fun x -> String.length x > 0)
        |> Array.of_list
        |> Array.fold_left (fun acc s -> acc ^ s) ""
        |> int_of_string
    | _ -> failwith "bad input"

let compute elt time = elt * (time - elt)

let binary_search x len =
    let rec _bin start last =
        if start >= last then start else
        let m = (start + last) / 2 in
        if (compute m len) < x then _bin ~++m last
        else _bin start m
    in _bin 0 len

let nb_solutions time dist =
    let start = binary_search dist time in
    let rec _adjust s =
        if compute s time = dist then _adjust ~++s else s
    in let start = _adjust start in
    (time - 2 * start + 1)

let part1 times dists =
    let rec _rec t d acc =
        match t, d with
        | e::q, a::b -> _rec q b (acc * nb_solutions e a)
        | _ -> acc
    in _rec times dists 1

let solve f =
    let l1 = input_line f in
    let l2 = input_line f in
    (part1 (get_list_of_int l1) (get_list_of_int l2),
    nb_solutions (get_one_int l1) (get_one_int l2))

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
