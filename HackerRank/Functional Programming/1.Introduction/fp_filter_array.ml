(* HackerRank challenge - Functional Programming - 1-6 - fp filter array *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec filter f ns = match ns with
    | [] -> []
    | x::xs -> (if f x then [x] else []) @ filter f xs

let () =
    let n::arr = read_lines() in
    let ans = filter (fun x -> x < n) arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;


