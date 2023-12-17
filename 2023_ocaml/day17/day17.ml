let file = "input"

let (~++) x = x+1
let (~--) x = x-1
let (>>>) x y = if x > y then x else y
let (<<<) x y = if x < y then x else y

let get_input f =
    let rec _get_line out =
        match input_line f with
        line -> _get_line (
            (line
            |> String.to_seq
            |> Array.of_seq
            |> Array.map
                (fun x -> int_of_char x - int_of_char '0')
            ):: out)
        | exception End_of_file -> List.rev out |> Array.of_list
    in _get_line []

module SetPoints = Set.Make(
    struct
        type t = int * int * int * int * int
        let compare = compare
    end
)

module MinHeap = struct
    type 'a t = Empty | Node of 'a * 'a t * 'a t

    let empty = Empty

    let is_empty = function
        | Empty -> true
        | _ -> false

    let rec insert x = function
        | Empty -> Node (x, Empty, Empty)
        | Node (y, left, right) ->
            if x <= y
            then Node (x, insert y right, left)
            else Node (y, insert x right, left)

    let find_min = function
        | Empty -> raise Not_found
        | Node (x, _, _) -> x

    let rec delete_min = function
        | Empty -> raise Not_found
        | Node (_, Empty, heap) | Node (_, heap, Empty) -> heap
        | Node (_, (Node (x, _, _) as left), (Node (y, _, _) as right)) ->
            if x <= y
            then Node (x, delete_min left, right)
            else Node (y, left, delete_min right)

    let pop_ref heap =
        let res = find_min !heap in
        heap := delete_min !heap; res

end

let add_values input point heap =
    let loss, row, col, delta_r, delta_c, n = point in
    let nr = row + delta_r and nc = col + delta_c in
    (if nr >= 0 && nr < Array.length input && nc >= 0 && nc < Array.length input.(nr) then
        heap := MinHeap.insert (loss + input.(nr).(nc), nr, nc, delta_r, delta_c, n + 1) !heap
    ); ()

let find_path input part2 =
    let seen = ref SetPoints.empty
    and heap = ref MinHeap.empty in
    heap := MinHeap.insert (0, 0, 0, 0, 0, 0) !heap;
    let rec _aux () =
        if MinHeap.is_empty !heap then failwith "end not found ??" else
        let loss, row, col, delta_r, delta_c, n = MinHeap.pop_ref heap in
        if row = Array.length input - 1
            && col = Array.length input.(row) - 1
            && (not part2 || n >= 4) then loss else
        if SetPoints.mem (row, col, delta_r, delta_c, n) !seen then _aux () else
        (
            seen := SetPoints.add (row, col, delta_r, delta_c, n) !seen;
            (if ((not part2 && n < 3) || (part2 && n < 10))
                && (delta_r, delta_c) <> (0, 0)
            then add_values input (loss, row, col, delta_r, delta_c, n) heap
            );
            let _x = [|(0, 1); (0, -1); (1, 0); (-1, 0)|] in
            for _i = 0 to 3 do
                let _r = fst _x.(_i) and _c = snd _x.(_i) in
                if (not part2 || n >= 4 || (delta_r, delta_c) = (0, 0))
                    && (_r, _c) <> (delta_r, delta_c)
                    && (_r, _c) <> (-delta_r, -delta_c) then
                add_values input (loss, row, col, _r, _c, 0) heap
            done;
            _aux ()
        )
    in _aux ()

let solve input = find_path input false, find_path input true

let () =
    let f = open_in file in
    let input = get_input f in
    close_in f;
    let res = solve input in
    "part1: " ^ (fst res |> string_of_int) ^ "\npart2: " ^ (snd res |> string_of_int)
    |> print_endline
