(* HackerRank challenge - Functional Programming - 2-1 - computing the gcd *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let rec gcd a b = if a mod b == 0 then b else gcd b (a mod b)

let () = Printf.printf "%d" (gcd (read_int ()) (read_int ())) 


