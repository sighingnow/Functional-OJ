(* HackerRank challenge - Functional Programming - 6-5 - kundu and bubble warp *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

(**
 * solution:
 * when there still `k` bubbles, the probability of select on bubble is k/n,
 * so, the expected times of this operation is n/k.
 * then, the expected number of seconds is \sum {n / k} <- k in [1..n].
 *)

let () = 
    let n = read_int () in
    let m = read_int () in
    let s = m * n in
    let ans = ref 0. in
    for i = 1 to s do
        ans := !ans +. (float_of_int s) /. (float_of_int i)
    done;
    Printf.printf "%f\n" !ans


