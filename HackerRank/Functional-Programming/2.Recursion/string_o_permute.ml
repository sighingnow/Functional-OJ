(* HackerRank challenge - Functional Programming - 2-6 string o permute *)

let read_string () = Scanf.scanf " %s" (fun x -> x)

let permute s =
    let l = String.length s in
    let ss = String.create l in
    let rec permute_tailrec s k all = match (all-k) with
        | -1   -> ss
        | 0    -> ss.[k] <- s.[all]; ss
        | _    -> ss.[k] <- s.[k+1]; ss.[k+1] <- s.[k]; permute_tailrec s (k+2) all
    in permute_tailrec s 0 (l-1)

let () = 
    let n = read_int () in
    for i = 1 to n do
        let s = read_string () in
        Printf.printf "%s\n" (permute s)
    done


