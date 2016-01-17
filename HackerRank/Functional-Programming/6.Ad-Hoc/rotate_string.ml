(* HackerRank challenge - Functional Programming - 6-2 - rotate string *)


let read_int _ = Scanf.scanf " %d" (fun x -> x)
let read_string () = Scanf.scanf " %s" (fun x -> x)

let () = 
    let t = read_int () in
    for i = 1 to t do
        let s = ref (read_string ()) in
        let n = String.length !s in
        for j = 1 to n do
            s := (String.sub !s 1 (n-1)) ^ (Char.escaped !s.[0]);
            Printf.printf "%s " !s;
        done;
        Printf.printf "\n";
    done;


