(* HackerRank challenge - Functional Programming - 6-1 - jumping bunnies *)

(* GCD and LCM. *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)
let rec gcd a b = match a, b with
    | a, 0 -> a
    | a, b -> gcd b (a mod b)

let lcm a b = (a / gcd a b) * b

let () = 
    let n = read_int () in
    let numbers = Array.init n read_int in
    let answer = Array.fold_left lcm 1 numbers in
    Printf.printf "%d\n" answer


