let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_input f =
    let rec _get_line out =
        match input_line f with
        line ->
            (String.to_seq line |> Array.of_seq) :: out
            |> _get_line
        | exception End_of_file -> List.rev out |> Array.of_list
    in _get_line []

let rotate_90_clockwise matrix =
    let rows = Array.length matrix
    and cols = Array.length matrix.(0) in
    let rotated = Array.make_matrix cols rows matrix.(0).(0) in
    for i = 0 to rows - 1 do
        for j = 0 to cols - 1 do
            rotated.(j).(rows - i - 1) <- matrix.(i).(j)
        done;
    done;
    rotated

let slide_rocks input =
    let m = rotate_90_clockwise input in
    Array.map (
        fun l ->
        let k = 
            Array.to_seq l
            |> String.of_seq
            |> String.split_on_char '#'
            |> List.map (fun x -> String.to_seq x |> Array.of_seq)
        in List.iter (Array.sort compare) k; k
            |> List.map (fun x -> Array.to_seq x |> String.of_seq)
            |> String.concat "#"
            |> String.to_seq
            |> Array.of_seq
    ) m

let count_blocks line i =
    Array.fold_left (
        fun acc l -> if l.(i) = 'O' then ~++acc else acc
    ) 0 line

let compute_total_load input =
    let len = Array.length input.(0) in
    let rec _inner c out =
        if c >= len then out else
        _inner ~++c (out + (~++c) * (count_blocks input c))
    in _inner 0 0

let compute_no_rotations input =
    let rec _inner grid c out =
        if c <= 0 then out else
        match grid with
        | [] -> out
        | e::q ->
            _inner q ~--c (out + c *
                (Array.fold_left
                    (fun acc ch ->
                        if ch = 'O' then acc + 1 else acc)
                    0 e)
            )
    in _inner (Array.to_list input) (Array.length input) 0

let cycle input =
    let m = ref input in
    for _ = 1 to 4 do
        m := slide_rocks !m
    done;
    !m

let board_to_string board =
    Array.map (
        fun l ->
            Array.to_seq l
            |> String.of_seq
    ) board
    |> Array.to_list
    |> String.concat "\n"

let string_to_board str =
    String.split_on_char '\n' str
    |> Array.of_list
    |> Array.map (fun l -> String.to_seq l |> Array.of_seq)

let cycles input nb_cycles =
    let h = (Hashtbl.create (1000))
    and str_input = board_to_string input in
    let all_boards = ref [| (str_input) |] in
    Hashtbl.add h str_input 0;
    let rec _inner curr board =
        let m = cycle board in
        let str_m = board_to_string m in
        if curr >= nb_cycles then (0, curr) else
        match Hashtbl.find h str_m with
        | x -> (x, curr)
        | exception Not_found ->
            all_boards := Array.append !all_boards [| str_m |];
            Hashtbl.add h str_m curr;
            _inner ~++curr m
    in 
    let first, len = _inner 1 input in
    !all_boards.(((nb_cycles - first) mod (len - first)) + first)
    |> string_to_board

let solve input =
    slide_rocks input |> compute_total_load,
    cycles input 1000000000 |> compute_no_rotations

let () =
    let f = open_in file in
    let input = get_input f in
    close_in f;
    let res = solve input in
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline;
