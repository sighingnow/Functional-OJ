(* HackerRank challenge - Functional Programming - 1-7 - array of n elements *)

let make_array n = Array.make n 1

let () =
  let len = read_int () in
  let arr = make_array len in
  Printf.printf "%d\n" (Array.length arr)


