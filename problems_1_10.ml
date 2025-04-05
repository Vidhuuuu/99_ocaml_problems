(* 1 - last element of a list *)
let rec last lst =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl
;;

(* 2 - penultimate element of a list *)
let rec last_two lst =
  match lst with
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: tl -> last_two tl

(* 3 - kth element of a list; List.nth_opt *)
let kth_elem lst k =
  let rec aux k = function
    | [] -> None
    | hd :: tl ->
        if k = 0 then Some hd
        else aux (k - 1) tl
  in
  aux k lst

(* 4 - number of elements in a list *)
let len_list lst =
  let rec aux acc = function
    | [] -> acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 0 lst

(* 5 - reverse a list; List.rev *)
let rec rev_list lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] lst

let lst = [1; 2; 3; 4; 5]

let () =
  List.iter (fun x -> Printf.printf "%d " x) (rev_list lst);
  print_newline ();
  Printf.printf "elem count: %d\n" (len_list lst);
