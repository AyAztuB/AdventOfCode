let str_to_list str =
  List.init (String.length str) (String.get str)
;;

let get_lines in_fd =
  let rec get_line output = 
    try
      let line = input_line in_fd in
      let line_list = str_to_list line in
      get_line (line_list :: output)
    with e ->
      match e with
        End_of_file -> List.rev output
        | _ -> raise e
  in get_line []
;;

(* To test and debug *)
let print_all_lines lines =
  let rec print_one_line l =
    match l with
      e::q -> let rec print_my_line _l =
                match _l with
                  a::b -> print_char a; print_my_line b
                  | _ -> print_endline ""
              in print_my_line e;
              print_one_line q
      | _ -> ()
  in print_one_line lines
;;

let is_digit s =
  if s >= '0' && s <= '9'
    then (int_of_char s) - (int_of_char '0')
  else -1
;;

let get_number line =
  let rec get_digit l =
    match l with
      e::q -> let n = (is_digit e) in
              if n >= 0
                then n
              else (get_digit q)
      | _ -> -1
  in let f1 = get_digit line in
  let f2 = get_digit (List.rev line) in
  f1 * 10 + f2
;;

let part1 all_lines =
  let rec compute line result =
    match line with
      e::q -> compute q (result + get_number e)
      | [] -> result
  in let res = compute all_lines 0 in
  print_string "part1: ";
  print_int res;
  print_endline ""
;;

let list_of_numbers = [ "one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine" ];;
let build_list_of_num l =
  let rec build_rec _l res =
    match _l with
      e::q -> build_rec q ((str_to_list e)::res)
      | _ -> res
  in build_rec l []
;;
let list_of_num = List.rev (build_list_of_num list_of_numbers);;

let is_equal line num =
  let rec _equal l n =
    match l,n with
      _, [] -> true
      | [], _ -> false
      | e::q, a::b -> if e == a then _equal q b else false
  in _equal line num
;;

let get_number_part2 line list_num =
  let rec get_digit l num i order =
    match l with
      e::q -> let n = (is_digit e) in
              if n >= 0
                then n
              else
                (match num with
                  a::b -> if order == 0 && is_equal l a
                            then i+1
                          else if order == 1 && is_equal l (List.rev a)
                            then i+1
                          else (get_digit l b (i + 1) order)
                  | _ -> get_digit q list_num 0 order)
      | _ -> -1
  in let f1 = get_digit line list_num 0 0 in
  let f2 = get_digit (List.rev line) list_num 0 1 in
  f1 * 10 + f2
;;

let part2 all_lines =
  let rec compute line result =
    match line with
      e::q -> compute q (result + get_number_part2 e list_of_num)
      | [] -> result
  in let res = compute all_lines 0 in
  print_string "part2: ";
  print_int res;
  print_endline ""
;;


let file = "input";;
let in_fd = open_in file;;
let all_lines = get_lines in_fd;;
let () =
  (* print_all_lines all_lines; *)
  part1 all_lines;
  part2 all_lines;
  close_in in_fd
;;
