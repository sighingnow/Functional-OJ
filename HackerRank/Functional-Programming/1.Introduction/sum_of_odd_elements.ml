(* HackerRank challenge - Functional Programming - 1-9 - sum of odd elements *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

let rec read_lines () =
    try let line = read_line () in
        int_of_string (line) :: read_lines()
    with
        End_of_file -> []

let rec sum_odd ns = match ns with
    | [] -> 0
    | x::xs -> (if x mod 2 = 1 or x mod 2 = -1 then x else 0) + sum_odd xs

let () =
    let arr = read_lines() in
    let ans = sum_odd arr in
    print_int ans


