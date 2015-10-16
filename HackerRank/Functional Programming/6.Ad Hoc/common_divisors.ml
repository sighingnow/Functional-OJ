(* HackerRank challenge - Functional Programming - 6-7 - common divisors *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)

(* define int map *)
module M = Map.Make(
    struct
        type t = int
        type key = int
        let compare = Pervasives.compare
    end
)

let get k m = try M.find k m with Not_found -> 0
let update k m = M.add k ((get k m) + 1) m

let divmap n = 
    let rec alldiv a b m =
        if a <= 1 then m
        else if a mod b = 0
            then alldiv (a/b) b (update b m)
            else alldiv a (b+1) m
    in
    alldiv n 2 M.empty;;

let () = 
    let cases = read_int () in
    for t = 1 to cases do
        let ans = ref 1 in
        let ma = divmap (read_int ()) in
        let mb = divmap (read_int ()) in
        M.iter (fun k v -> let e = get k ma in
            ans := !ans * ((min v e) + 1)
        ) mb;
        Printf.printf "%d\n" !ans;
    done;;
