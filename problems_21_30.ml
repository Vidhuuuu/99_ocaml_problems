(* 21 - insert elem *)
let insert_at elem n lst =
  let rec aux acc i = function
    | [] -> acc
    | hd :: tl ->
        if i = n then aux (hd :: elem :: acc) (i + 1) tl
        else aux (hd :: acc) (i + 1) tl
  in
  List.rev (aux [] 0 lst)
;;

(* 22 - range *)
let range s e =
  let rec aux acc i =
    if i = e then (i :: acc)
    else aux (i :: acc) (i + 1)
  in
  List.rev (aux [] s)
;;

(* 23 - random elems *)
let rand_select lst n =
  Random.self_init ();
  let rec aux acc i =
    if i = n then acc
    else
      let num = Random.int_in_range ~min:0 ~max:(List.length lst - 1) in
      aux ((List.nth lst num) :: acc) (i + 1)
  in
  List.rev (aux [] 0)
;;

(* 24 - lotto *)
(*
  let lotto_select n m =
    Random.self_init ();
    let rec aux acc i =
      if i = n then acc
      else
        let num = Random.int_in_range ~min:1 ~max:m in
        aux (num :: acc) (i + 1)
    in
    List.rev (aux [] 0)
  ;;
*)
let lotto_select n m =
  Random.self_init ();
  let all_nums = List.init m (fun x -> x + 1) in
  let shuffled = List.sort (fun _ _ -> Random.int 3 - 1) all_nums in
  let rec aux acc lst i =
    if i = n then acc
    else
      match lst with
      | [] -> failwith "Inadequate elements"
      | hd :: tl -> aux (hd :: acc) tl (i + 1)
  in
  aux [] shuffled 0
;;

(* 25 - random permutation *)
(*
  let permutation lst =
    Random.self_init ();
    let arr = Array.of_list lst in
    let len = Array.length arr in
    for i = len - 1 downto 1 do
      let j = Random.int (i + 1) in
      let tmp = arr.(i) in
      arr.(i) <- arr.(j);
      arr.(j) <- tmp
    done;
    Array.to_list arr
  ;;
*)
let permutation lst =
  let rec extract acc i = function
    | [] -> raise Not_found
    | hd :: tl ->
        if i = 0 then (hd, List.rev_append acc tl)
        else extract (hd :: acc) (i - 1) tl
  in
  let extract_rand lst len =
    extract [] (Random.int len) lst
  in
  let rec aux acc lst len =
    if len = 0 then acc
    else
      let taken, rest = extract_rand lst len in
      aux (taken :: acc) rest (len - 1)
  in
  Random.self_init ();
  aux [] lst (List.length lst)
;;

(* 26 - combinations *)
let rec extract k lst =
  match k, lst with
  | 0, _ -> [[]]
  | _, [] -> []
  | k, hd :: tl ->
      let with_hd =
        extract (k - 1) tl
        |> List.map (fun rest -> hd :: rest)
      in
      let without_hd = extract k tl in
      with_hd @ without_hd
;;

(* 27 - group into disjoint subsets *)
(* couldn't figure out, yet*)

(* 28 - length and frequency sorting *)
let length_sort lst =
  List.sort (fun lst1 lst2 -> List.length lst1 - List.length lst2) lst
;;

(*
  let frequency_sort lst =
    let count_elem lst len =
      List.fold_left (fun acc x ->
        if List.length x = len then acc + 1 else acc) 0 lst
    in
    List.sort (fun lst1 lst2 ->
      let len_lst1 = List.length lst1 in
      let len_lst2 = List.length lst2 in
      compare (count_elem lst len_lst1) (count_elem lst len_lst2)) lst 
  ;;
*)
let frequency_sort lst =
  let freq = Hashtbl.create 32 in
  List.iter (fun sublst ->
    let len = List.length sublst in
    Hashtbl.replace freq len (1 + (Hashtbl.find_opt freq len
        |> Option.value ~default:0))
  ) lst;
  List.sort (fun lst1 lst2 ->
    let len_lst1 = List.length lst1 in
    let len_lst2 = List.length lst2 in
    compare (Hashtbl.find freq len_lst1) (Hashtbl.find freq len_lst2)
  ) lst
;;

let lst = ["a"; "b"; "c"; "d"]
let lst2 = ["a";"b";"c";"d";"e";"f";"g";"h"]
let lst3 = [["a";"b";"c"]; ["d";"e"]; ["f";"g";"h"]; ["d";"e"];
                ["i";"j";"k";"l"]; ["m";"n"]; ["o"]]

let () =
  (* List.iter (fun x -> Printf.printf "%s " x) (lst); *)
  (* print_newline () *)
  List.iter (fun x ->
    List.iter (fun y ->
      Printf.printf "%s " y) x; print_newline ()) (frequency_sort lst3)
