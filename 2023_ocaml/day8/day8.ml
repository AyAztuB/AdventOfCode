let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_key_value_line line =
    match String.split_on_char '=' line with
    | key :: values :: [] -> (
            match String.split_on_char ',' values with
            | a :: b :: [] ->
                ((String.sub key 0 3), ((String.sub a 2 3), (String.sub b 1 3)))
            | _ -> failwith "bad input"
        )
    | _ -> failwith "bad input"

let get_input f htbl =
    let rec _get_line start_p2 =
        match input_line f with
        line ->
            let key, values = get_key_value_line line in
            Hashtbl.add htbl key values;
            _get_line (if String.ends_with ~suffix:"A" key then key::start_p2 else start_p2)
        | exception End_of_file -> start_p2
    in _get_line []

let follow_dirs dirs start htbl is_end_fun =
    let rec _goto_end idx curr count =
        if is_end_fun curr then count else
        let n = Hashtbl.find htbl curr in
        _goto_end
            (if ~++idx >= String.length dirs then 0 else ~++idx)
            (if dirs.[idx] = 'L' then fst n else snd n)
            ~++count
    in _goto_end 0 start 0

let part1 htbl dirs =
    follow_dirs dirs "AAA" htbl (fun x -> x = "ZZZ")

let get_all_ends htbl dirs starts =
    let rec _get s out =
        match s with
        (* We are assuming that there is a cycle so time to go to the first end is also time to go to n-th end from (n-1)-th end *)
        | e :: q -> _get q ((follow_dirs dirs e htbl (String.ends_with ~suffix:"Z"))::out)
        | _ -> out
    in _get starts []

let rec gcd a b =
    if b = 0 then a else gcd b (a mod b)

let compute_lcm outs =
    match outs with
    | e :: q -> (
            let rec _compute o res =
                match o with
                | a::b -> _compute b (res * a / (gcd res a))
                | _ -> res
            in _compute q e
        )
    | _ -> failwith "Empty ??"

let part2 htbl dirs starts =
    get_all_ends htbl dirs starts |> compute_lcm

let solve f =
    let dirs = input_line f in
    let _ = input_line f in
    let htbl = Hashtbl.create 1000 in
    let start_p2 = get_input f htbl in
    (part1 htbl dirs), (part2 htbl dirs start_p2)

let () =
    let f = open_in file in
    let res = solve f in
    close_in f;
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
