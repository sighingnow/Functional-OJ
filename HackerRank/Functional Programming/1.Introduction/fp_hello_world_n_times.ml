(* HackerRank challenge - Functional Programming - 1-3 - fp hello world n times *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let () = 
    let n = read_int () in
    for i = 1 to n do
        print_string "Hello World\n";
    done
