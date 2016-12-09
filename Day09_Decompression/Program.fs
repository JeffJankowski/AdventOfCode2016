// http://adventofcode.com/2016/day/9
// 12/08/2016
//

open System
open System.IO
open System.Text.RegularExpressions
open System.Numerics


let rec decompr (input : string) func =
    let cnt = input |> Seq.takeWhile (fun c -> c <> '(') |> Seq.length
    if cnt = input.Length then bigint cnt else
        let mrk = input |> Seq.skip (cnt+1) |> Seq.takeWhile (fun c -> c <> ')') |> String.Concat
        let [nchars; times] = List.tail [for g in (Regex.Match (mrk, @"(\d+)x(\d+)")).Groups -> g.Value] |> List.map int
        bigint cnt 
            + (bigint times * (func input (cnt + mrk.Length + 2) nchars)) 
            + (decompr (input |> Seq.skip (cnt + mrk.Length + 2 + nchars) |> String.Concat) func)


[<EntryPoint>]
let main argv = 
    let input = File.ReadAllText ("..\..\input.txt")
    
    printfn "Original format length:  %A" (decompr input (fun _ _ nchars -> bigint nchars))
    let rec f (s : string) k n = decompr (s |> Seq.skip k |> Seq.take n |> String.Concat) f
    printfn "Improved format length:  %A" (decompr input f)

    Console.Read ()