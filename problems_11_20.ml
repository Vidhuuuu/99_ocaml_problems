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

let lst = ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
let lst2 = [Many (4,"a"); One "b"; Many (2,"c"); Many (2,"a"); One "d"; Many (4,"e")]

let () =
  List.iter (fun x -> Printf.printf "%s " x) (decode lst2);
  print_newline ()
