// http://adventofcode.com/2016/day/15
// 12/14/2016

open System
open System.IO
open Helpers

[<EntryPoint>]
let main argv = 
    let discs = 
        File.ReadAllLines ("..\\..\\input.txt")
        |> Array.map (fun s ->
            match s with
            | Regex @"Disc #\d has (\d+) positions; at time=0, it is at position (\d+)." 
                [n; pos] -> (int pos, int n) )

    let target = Array.mapi (fun i (p,n) -> modulo (n-i-1) n, n) discs

    Seq.initInfinite id
    |> Seq.scan (fun acc _ -> Array.map (fun (p,n) -> ((p + 1) % n, n)) acc) discs
    |> Seq.findIndex (fun state -> state = target)
    |> printfn "Drop capsule when t=%i"

    Console.Read ()
