// http://adventofcode.com/2016/day/16
// 12/16/2016

open System
open System.IO
open Helpers
open System.Diagnostics

let rec dragon (input: string) n =
    if (input.Length >= n) then input.Substring(0, n) else
    dragon (input + "0" + (input.ToCharArray() |> Array.rev 
        |> String.Concat).Replace("0","x").Replace("1","0").Replace("x", "1")) n

let rec checksum (input: string) = 
    let cs = 
        input.ToCharArray()
        |> Seq.pairwise
        |> Seq.mapi (fun i x -> i,x)
        |> Seq.filter (fun (i,_) -> i % 2 = 0)
        |> Seq.map (fun (_,(x,y)) -> if x = y then "1" else "0")
        |> String.Concat
    if cs.Length % 2 = 1 then cs else checksum cs

[<EntryPoint>]
let main argv =  
    let sw = Stopwatch.StartNew();
    printfn "Checksum: %s" (checksum (dragon "01111001100111011" 35651584))
    sw.Stop ()
    printfn "Elapsed: %f sec" (float sw.ElapsedMilliseconds / 1000.0)
    Console.Read ()