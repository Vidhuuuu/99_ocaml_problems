(* 11 - modified RLE *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let make_tuple cnt elem =
  if cnt = 1 then One elem
  else Many (cnt, elem)

let encode lst =
  let rec aux acc cnt = function
    | [] -> acc
    | [hd] -> (make_tuple (cnt + 1) hd) :: acc
    | hd1 :: (hd2 :: _ as tl) ->
        if hd1 = hd2 then aux acc (cnt + 1) tl
        else aux ((make_tuple (cnt + 1) hd1) :: acc) 0 tl
  in
  List.rev (aux [] 0 lst)
;;

(* 12 - decode RLE *)
let rec nadd_to lst n x =
  if n = 0 then lst 
  else nadd_to (x :: lst) (n - 1) x

let decode lst =
  let rec aux acc = function
    | [] -> acc
    | One x :: tl -> aux (x :: acc) tl
    | Many (cnt, x) :: tl -> aux (nadd_to acc cnt x) tl
  in
  List.rev (aux [] lst)
;;

(* 13 - RLE direct *)
(* same as 11? *)

(* 14 - duplicate elems of a list *)
let duplicate lst =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (hd :: hd :: acc) tl
  in
  List.rev (aux [] lst)
;;

(* 15 - replicate elems n times *)
let replicate lst n =
  let rec aux acc = function
    | [] -> acc
    | hd :: tl -> aux (nadd_to acc n hd) tl
  in
  List.rev (aux [] lst)
;;

(* 16 - drop nth *)
let drop lst n =
  let rec aux acc curr = function
    | [] -> acc
    | hd :: tl ->
        if curr = 1 then aux acc n tl
        else aux (hd :: acc) (curr - 1) tl
  in
  List.rev (aux [] n lst)
;;

(* 17 - split two lists *)
let split lst n =
  let rec aux acc i = function
    | [] -> (List.rev acc, [])
    | hd :: tl as li ->
        if i = 0 then (List.rev acc, li)
        else aux (hd :: acc) (i - 1) tl
  in
  aux [] n lst
;;

(* 18 - extract slice *)
let slice lst i k =
  let rec aux acc curr = function
    | [] -> acc
    | hd :: tl ->
        if curr >= i && curr <= k then aux (hd :: acc) (curr + 1) tl
        else aux acc (curr + 1) tl
  in
  List.rev (aux [] 0 lst)
;;


let lst = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let lst2 = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]
let lst3 = ["a";"b";"c";"c";"d"]
let lst4 = ["a";"b";"c";"d";"e";"f";"g";"h";"i";"j"]

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
  List.iter (fun x -> Printf.printf "%s " x) (slice lst4 2 6);
  print_newline ()
  (* match (split lst4 4) with *)
  (* | (x, y) -> print_list print_string x; print_list print_string y *)
