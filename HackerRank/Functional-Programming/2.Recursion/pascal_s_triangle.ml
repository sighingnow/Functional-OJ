(* HackerRank challenge - Functional Programming - 2-3 - pascal's triangle *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let triangle = Array.init 15 (fun _ -> 0)

let pascal n = 
    let rec pascal_rec n = match n with
        | 1 ->
            let xs = Array.init (n+2) (fun _ -> 0) in
            Array.set xs 1 1;
            for i = 1 to n do
                Printf.printf "%d " (Array.get xs i);
            done;
            Printf.printf "\n";
            xs
        | n ->
            let top = pascal_rec (n-1) in
            let xs = Array.init (n+2) (fun _ -> 0) in
            for i = 1 to n do
                Array.set xs i ((Array.get top (i-1)) + (Array.get top i));
                Printf.printf "%d " (Array.get xs i);
            done;
            Printf.printf "\n";
            xs
    in
    let top = pascal_rec n in
    Printf.printf ""

let () = pascal (read_int ())


