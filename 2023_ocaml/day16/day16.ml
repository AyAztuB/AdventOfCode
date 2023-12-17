let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_input f =
    let rec _get_line out =
        match input_line f with
        line -> _get_line (line :: out)
        | exception End_of_file -> List.rev out |> Array.of_list
    in _get_line []

module SetPoints = Set.Make(struct type t = int * int * int * int let compare = compare end)

let add point q seen =
    if not @@ SetPoints.mem point !seen
    then (seen := SetPoints.add point !seen;
    Queue.add point !q); ()

let calculate start_point input =
    let q = ref (Queue.create ())
    and seen = ref (SetPoints.empty) in
    Queue.add start_point !q;
    let rec _aux () =
        if Queue.is_empty !q then !seen else
        let row, col, delta_r, delta_c = Queue.pop !q in
        let r = row + delta_r and c = col + delta_c in
        if r < 0 || r >= Array.length input
            || c < 0 || c >= String.length input.(r) then _aux () else
        match input.(r).[c] with
        | ch when ch = '.'
            || (ch = '-' && delta_c <> 0)
            || (ch = '|' && delta_r <> 0) ->
            add (r, c, delta_r, delta_c) q seen;
            _aux ()
        | '/' ->
            add (r, c, -delta_c, -delta_r) q seen;
            _aux ()
        | '\\' ->
            add (r, c, delta_c, delta_r) q seen;
            _aux ()
        | '|' ->
            add (r, c, 1, 0) q seen;
            add (r, c, -1, 0) q seen;
            _aux ()
        | '-' ->
            add (r, c, 0, 1) q seen;
            add (r, c, 0, -1) q seen;
            _aux ()
        | _ -> failwith "bad input"
    in _aux ()
    |> SetPoints.map (fun x -> let r, c, _, _ = x in (r, c, 0, 0))
    |> SetPoints.cardinal

let part2 input =
    let rec _all_rows i res =
        if i >= Array.length input then res else
        let f = calculate (i, -1, 0, 1) input
        and l = calculate (i, String.length input.(i), 0, -1) input in
        _all_rows ~++i (res >>> (f >>> l))
    in let rec _all_cols i res =
        if i >= String.length input.(0) then res else
        let f = calculate (-1, i, 1, 0) input
        and l = calculate (Array.length input, i, -1, 0) input in
        _all_cols ~++i (res >>> (f >>> l))
    in (_all_cols 0 0) >>> _all_rows 0 0

let solve input =
    calculate (0, -1, 0, 1) input,
    part2 input

let () =
    let f = open_in file in
    let input = get_input f in
    close_in f;
    let res = solve input in
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
