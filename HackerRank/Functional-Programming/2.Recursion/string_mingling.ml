(* HackerRank challenge - Functional Programming - 2-5 - string mingling *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)
let read_string _ = Scanf.scanf " %s" (fun x -> x)

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

let mingling p q = 
    List.concat (List.map (fun (a, b) -> [a;b]) (List.combine p q))

let () = 
    let a = explode (read_string ()) in
    let b = explode (read_string ()) in
    Printf.printf "%s" (implode (mingling a b))


