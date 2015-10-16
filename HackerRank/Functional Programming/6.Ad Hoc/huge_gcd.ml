(* HackerRank challenge - Functional Programming - 6-4 - huge gcd *)

let read_int () = Scanf.scanf " %d" (fun x -> x);;
let modbase = 1000000007L;;

let primea = Array.init 10005 (fun _ -> 0)
let primeb = Array.init 10005 (fun _ -> 0)

let handle (a: int ref) cnt =
    let b = ref 2 in
    while !a > 1 do
        if (!a mod !b) = 0 then begin
            Array.set cnt !b (Array.get cnt !b + 1);
            a := !a / !b;
        end
        else b := !b + 1;
    done

let int64_mod (a: int64 ref) (b: int64) = 
    while compare !a b > 0 do
        a := Int64.sub !a b;
    done;
    !a

let accumulate (acc: int64 ref) idx = 
    let m = min (Array.get primea idx) (Array.get primeb idx) in
    let base = (Int64.of_int idx) in
    for i = 1 to m do
        acc := int64_mod (ref (Int64.mul !acc base)) modbase;
    done;
    !acc

let () = 
    let n = read_int () in
    for i = 1 to n do
        handle (ref (read_int ())) primea;
    done;
    let m = read_int () in
    for i = 1 to m do
        handle (ref (read_int ())) primeb;
    done;
    let ans = ref 1L in
    for i = 1 to 10000 do
        ans := accumulate ans i;
    done;
    Printf.printf "%s\n" (Int64.to_string !ans)



