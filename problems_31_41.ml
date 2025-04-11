(* 31 - is prime *)
(* Sieve of Eratosthenes *)
let sieve n =
  let rec aux = function
    | [] -> []
    | hd :: tl ->
        hd :: aux (List.filter (fun x -> x mod hd <> 0) tl)
  in
  aux (List.init (n + 1) (fun x -> x + 2))
;;

let is_prime n =
  let rec check d =
    d * d > n || (n mod d <> 0 && check (d + 1))
  in
  n > 1 && check 2
;;

(* 32 - GCD, Euclidean Algorithm *)
let rec gcd x y =
  if y = 0 then x
  else gcd y (x mod y)
;;

(* 33 - coprime *)
let coprime x y =
  gcd x y = 1
;;

(* 34 - euler's toitent function *)
let phi m =
  let rec aux acc d =
    if d = m then acc
    else
      let new_acc = if coprime m d then acc + 1 else acc in
      aux new_acc (d + 1)
  in
  aux 0 1
;;

(* 35 - prime factors *)
let factors n =
  let primes_lst = sieve n in
  let rec aux acc n = function
    | [] -> if n = 1 then acc else n :: acc
    | hd :: tl as curr ->
        if n mod hd = 0 then aux (hd :: acc) (n / hd) curr 
        else aux acc n tl
  in
  List.rev (aux [] n primes_lst)
;;

(* 36 - prime factors with multiplicity *)
(*
  A way to do this but not the best
  let factors_count n =
    let primes_lst = sieve n in
    let rec aux acc n = function
      | [] -> acc
      | hd :: tl ->
          if n mod hd = 0 then
            let rec count_divisors n count =
              if n mod hd = 0 then count_divisors (n / hd) (count + 1)
              else (n, count)
            in
            let (new_n, count) = count_divisors n 0 in
            aux ((hd, count) :: acc) new_n tl
          else
            aux acc n tl
    in
    List.rev (aux [] n primes_lst)
  ;;
*)
let factors_count n =
  let rec aux d n =
    if n = 1 then []
    else
      if n mod d = 0 then
        match aux d (n / d) with
        | (x, y) :: tl when x = d -> (x, y + 1) :: tl
        | l -> (d, 1) :: l
      else
        aux (d + 1) n
  in
  aux 2 n
;;

(* 37 - euler's toitent function improved *)
(*
  phi(n) = n * (1 - 1/x1) * (1 - 1/x2) * ...
  for x1, x2,... as prime factors
*)
let phi_improved n =
  let factors_lst = factors_count n in
  List.fold_left (fun acc (x, _) -> acc * (x - 1) / x) n factors_lst
;;

(* 38 - compare both euler toitent function implementations *)
let timeit f1 f2 n =
  let t1 = Sys.time() in
  let _ = f1 n in
  Printf.printf "time for f1: %fs\n" (Sys.time() -. t1);
  let t2 = Sys.time() in
  let _ = f2 n in
  Printf.printf "time for f2: %fs\n" (Sys.time() -. t2)
;;

(* 39 - primes in range *)
let all_primes l h =
  let rec aux acc i =
    if i > h then acc
    else
      if is_prime i then aux (i :: acc) (i + 1)
      else aux acc (i + 1)
  in
  List.rev (aux [] l)
;;

(* 40 - golbach's conjecture *)
let goldbach n =
  let rec aux = function
    | [] -> raise Not_found
    | hd :: tl ->
        if is_prime (n - hd) then (hd, (n - hd))
        else aux tl
  in
  aux (sieve n)
;;

(* 41 - list of goldbach compositions *)
let goldbach_lst l h =
  let rec aux acc i =
    if i > h then acc
    else
      if i mod 2 = 1 then aux acc (i + 1)
      else aux ((i, goldbach i) :: acc) (i + 1)
  in
  List.rev (aux [] l)
;;

let () =
  List.iter (fun (x, (y, z)) -> Printf.printf "(%d, (%d, %d))\n" x y z) (goldbach_lst 9 20)
