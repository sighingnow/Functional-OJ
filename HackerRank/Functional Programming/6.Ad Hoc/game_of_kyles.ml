(* HackerRank challenge - Functional Programming - 6-9 game of kyles *)

let read_int _ = Scanf.scanf " %d" (fun x -> x)
let read_string _ = Scanf.scanf " %s" (fun x -> x)

(**
 * solution:
 * game theory, Sprague-Grundy value. more datails: see @ 
 *      https://www.hackerrank.com/contests/lambda-calculi-may14/challenges/game-of-kyles/forum
 *
 * Let's represent grundy value for k pins by g(k). Suppose, there are n pins initially, in a row. 
 *
 * So theyre are multiple options.
 * 1. You stuck 1 pin on either side. Then new grundy value will be g(k-1).
 * 2. You stuck 2 pin on either side. Then new grundy value will be g(k-2).
 * 3. You can stuck any one pin in the non-end pins. Suppose hit pin at i'th position. So pins will 
 *    be divided into two groups of size (i-1) and (n-i) pins. So it yeild the grundy value of g(i) 
 *    XOR g(n-1). Here i lies in [2,N-1].
 * 4. You stuck two consequetive pins, which are not at either ends. Suppose their index is j and j+1.
 *    Then we will have two groups of pins, where first group will contain j-1 pins and second will 
 *    contain n-j-1 pins. So new groundy value will be g(j) XOR g(n-j-1). Here range of j will be in 
 *    [2, N-2].
 * 
 * So grundy value for n consequtive pins will be
 *
 *      g(n) = mex(g(n-1), g(n-2), g(i) XOR g(n-i), g(j) XOR g(n-j-i))
 * 
 *      where
 *          2 <= i <= n-1
 *          2 <= j <= n-2
 *          mex = minimal excludent.
 *)

let explode s = 
    let rec exp i l = 
        if i < 0 then l else exp (i-1) (s.[i] :: l)
    in
    exp (String.length s - 1) []

let cache = Array.make 305 (-1) (* memorization *)

let rec sg n = match n with
    | -1 | 0 -> 0
    | _ -> if cache.(n) <> -1 then cache.(n) else begin
        let mark = Array.make 305 false in
        for j = 0 to n-1 do
            mark.((sg j) lxor (sg (n-j-1))) <- true;
            mark.((sg j) lxor (sg (n-j-2))) <- true;
        done;
        let ret = ref 0 in
        while mark.(!ret) = true do
            incr ret;
        done;
        cache.(n) <- !ret;
        !ret
    end


let () = 
    let cases = read_int () in
    for case = 1 to cases do
        let _ = read_int () in
        let s = explode (read_string ()) in
        let (grundy, size) = List.fold_right (fun ch (grundy, size) ->
            if ch = 'I'
                then (grundy, size + 1)
                else (grundy lxor (sg size), 0))
            s (0, 0)
        in
        let grundy = grundy lxor (sg size) in
        let ans = match grundy with
            | 0 -> "LOSE"
            | _ -> "WIN"
        in
        Printf.printf "%s\n" ans;
    done

