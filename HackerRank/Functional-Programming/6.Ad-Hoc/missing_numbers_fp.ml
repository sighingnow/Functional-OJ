(* HackerRank challenge - Functional Programming - 6-6 - missing numbers (fp) *)

let read_int _ = Scanf.scanf " %d" (fun x -> x);;

(* define int ordered map. *)
module M = Map.Make(
    struct
        let compare = Pervasives.compare
        type t = int
        type key = int
    end
)

let get k m = try M.find k m with Not_found -> 0
let update k m = M.add k ((get k m) + 1) m

let array_to_map arr = Array.fold_left (fun m x -> update x m) M.empty arr

let () = 
    let n = read_int () in
    let mapA = array_to_map (Array.init n read_int) in
    let m = read_int () in
    let mapB = array_to_map (Array.init m read_int) in
    M.iter(fun k v -> if ((get k mapA) < (get k mapB)) then Printf.printf "%d " k) mapB

