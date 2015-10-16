(* HackerRank challenge - Functional Programming - 1-6 - filter positions in a list *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec filter idx ns = match ns with
    | [] -> []
    | x::xs -> (if idx mod 2 = 0 then [x] else []) @ filter (idx + 1) xs

let () =
    let arr = read_lines() in
    let ans = filter 1 arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;


