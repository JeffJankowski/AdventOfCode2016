﻿module Helpers
open System.Text.RegularExpressions



// real modulus that workds with negatives instead of .NET's crap
let modulo n m = ((n % m) + m) % m


let chunkBy n s =
    seq {
        let r = ResizeArray<_>()
        for x in s do
            r.Add(x)
            if r.Count = n then
                yield r.ToArray()
                r.Clear()
        if r.Count <> 0 then
            yield r.ToArray()
    }



let (|Regex|_|) pattern input =
    let m = Regex.Match(input, pattern)
    if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
    else None


// F# for Scientists (page 166-167)
//*********************************
let rec distribute e = function
  | [] -> [[e]]
  | x::xs' as xs -> (e::xs)::[for xs in distribute e xs' -> x::xs]
let rec permute = function
  | [] -> [[]]
  | e::xs -> List.collect (distribute e) (permute xs)
//*********************************


let rec comb n l = 
    match n, l with
    | 0, _ -> [[]]
    | _, [] -> []
    | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs


let rec powerset s = seq {
    match s with
    | [] -> yield []
    | h::t -> for x in powerset t do yield! [x; h::x] }