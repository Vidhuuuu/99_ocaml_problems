(* 1 - last element of a list *)
let rec last lst =
  match lst with
  | [] -> None
  | [x] -> Some x
  | _ :: tl -> last tl
;;

(* 2 - last two elements of a list *)
let rec last_two lst =
  match lst with
  | [] | [_] -> None
  | [a; b] -> Some (a, b)
  | _ :: tl -> last_two tl
;;

(* 3 - kth element of a list; List.nth_opt *)
let kth_elem lst k =
  let rec aux k = function
    | [] -> None
    | hd :: tl ->
        if k = 0 then Some hd
        else aux (k - 1) tl
  in
  aux k lst
;;

(* 4 - number of elements in a list; List.length *)
let len_list lst =
  let rec aux acc = function
    | [] -> acc
    | _ :: tl -> aux (acc + 1) tl
  in
  aux 0 lst
;;

(* 5 - reverse a list; List.rev *)
let rec rev_list lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: acc) tl
  in
  aux [] lst
;;

(* 6 - check palindrome *)
let is_palindrome lst =
  let rev_lst = rev_list lst in
  rev_lst = lst
;;

(* 7 - flatten a nested list *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: tl -> aux (x :: acc) tl
    | Many xs :: tl -> aux (aux acc xs) tl
  in
  List.rev (aux [] lst)
;;

(* 8 - eliminate consecutive duplicates *)
let compress lst =
  let rec aux acc = function
    | [] -> acc
    | [hd] -> hd :: acc
    | hd1 :: (hd2 :: _ as tl) ->
        if hd1 = hd2 then aux acc tl
        else aux (hd1 :: acc) tl
  in
  List.rev (aux [] lst)
;;

(* 9 - pack consecutive duplicates *)
let pack lst =
  let rec aux acc curr = function
    | [] -> acc
    | [hd] -> (hd :: curr) :: acc
    | hd1 :: (hd2 :: _ as tl) ->
        if hd1 = hd2 then aux acc (hd1 :: curr) tl
        else aux ((hd1 :: curr) :: acc) [] tl
  in
  List.rev (aux [] [] lst)
;;

(* 10 - RLE *)
let encode lst =
  let rec aux acc cnt = function
    | [] -> acc
    | [hd] -> (cnt + 1, hd) :: acc
    | hd1 :: (hd2 :: _ as tl) ->
        if hd1 = hd2 then aux acc (cnt + 1) tl
        else aux ((cnt + 1, hd1) :: acc) 0 tl
  in
  List.rev (aux [] 0 lst)
;;

let lst = [1; 2; 3; 4; 5]
let lst2 = [1; 2; 4; 5; 4; 2]
let lst3 = ["a"; "c"; "b"; "d"; "e"; "z"]
let lst4 = [ One "a" ; Many [ One "b" ; Many [ One "c" ; One "d" ] ; One "e" ] ]
let lst5 = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let lst6 = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]

let print_list f lst =
  print_string "[";
  let rec aux = function
    | [] -> ()
    | [x] -> f x
    | x :: xs ->
        f x;
        print_string "; ";
        aux xs
  in
  aux lst;
  print_endline "]"
;;

let () =
  List.iter (fun x -> print_list print_string x) (pack lst5);
  List.iter (fun (cnt, elem) -> Printf.printf "%d: %s\n" cnt elem) (encode lst6);
