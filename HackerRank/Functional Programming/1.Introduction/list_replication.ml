(* HackerRank challenge - Functional Programming - 1-4 - list replication *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let f n arr = 
    let rec repeat x n = match n with
        | 0 -> []
        | n -> x :: repeat x (n-1)
    in
    List.concat (List.map (fun x -> repeat x n) arr)

let () =
    let n::arr = read_lines() in
    let ans = f n arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;


