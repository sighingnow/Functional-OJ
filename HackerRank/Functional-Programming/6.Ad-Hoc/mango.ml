(* HackerRank challenge - Functional Programming - 6-8 - mango *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

(**
 * solution:
 * binary search.
 *)

let () = 
    let n = read_int () in
    let m = read_int () in
    let a = Array.init n read_int in
    let h = Array.init n read_int in
    let test k =
        let b = Array.mapi (fun i x -> x + (k-1) * h.(i)) a in
        Array.fast_sort compare b;
        let s = Array.fold_left (fun acc x -> acc + x) 0 (Array.sub b 0 k) in
        s <= m
    in
    (* binary search *)
    let rec bs lo hi =
        if lo = hi - 1 then lo
        else begin
            let mid = (lo + hi) / 2 in
            if test mid then bs mid hi else bs lo mid
        end
    in
    Printf.printf "%d\n" (bs 0 (n+1))
