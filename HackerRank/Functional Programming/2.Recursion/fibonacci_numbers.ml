(* HackerRank challenge - Functional Programming - 2-2 - fibonacci numbers *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let fib n = 
    let rec fib_tailrec n a b = match n with
        | 1 -> a
        | 2 -> b
        | n -> fib_tailrec (n-1) b (a+b)
    in fib_tailrec n 0 1

let () = Printf.printf "%d" (fib (read_int ())) 


