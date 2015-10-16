(* HackerRank challenge - Functional Programming - 6-10 subset sum *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

(**
 * solution:
 * binary search.
 *)

let () = 
    let n = read_int () in
    let ns = Array.init n read_int in
    let prefix = Array.make n 0 in
    Array.fast_sort (fun l r -> r - l) ns;
    Array.fold_left (fun (s, idx) x -> Array.set prefix idx (s+x); (s+x, idx+1)) (0, 0) ns;
    let rec bs lo hi k = 
        if prefix.(hi-1) < k then (-2)
        else if lo = hi - 1 then lo
        else begin
            let mid = (lo + hi) / 2 in
            if prefix.(mid-1) >= k
                then bs lo mid k
                else bs mid hi k
        end
    in
    let t = read_int () in
    for i = 1 to t do
        Printf.printf "%d\n" (bs 0 n (read_int ()) + 1)
    done;

