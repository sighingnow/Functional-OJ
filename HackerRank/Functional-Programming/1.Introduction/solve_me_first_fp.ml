(* HackerRank challenge - Functional Programming - 1-1 - solve me first fp *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let () = 
    print_int (read_int () + read_int ())
