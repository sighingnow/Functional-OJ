(* HackerRank challenge - Functional Programming - 6-3 - remove duplicates *)

let read_string () = Scanf.scanf " %s" (fun x -> x)

(* convert string to list of chars *)
let explode s =
    let rec exp i l =
        if i < 0 then l else exp (i-1) (s.[i] :: l) in
    exp (String.length s - 1) []

(* convert list of chars to string *)
let implode l = 
    let result = String.create (List.length l) in
    let rec imp i = function
        | [] -> result
        | c :: l -> result.[i] <- c; imp (i+1) l in
    imp 0 l

let () = 
    let s = read_string () in
    let ans = List.fold_left (fun acc c -> if (List.mem c acc) then acc else (acc @ [c])) [] (explode s) in
    Printf.printf "%s" (implode ans)


