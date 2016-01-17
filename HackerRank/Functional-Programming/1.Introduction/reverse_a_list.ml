(* HackerRank challenge - Functional Programming - 1-8 - reverse a list *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let () =
    let arr = read_lines() in
    let ans = List.rev arr in
    List.iter (fun x -> print_int x; print_newline ()) ans;;


