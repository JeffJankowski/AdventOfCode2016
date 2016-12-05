// http://adventofcode.com/2016/day/3
// 12/05/2016
//

open System
open System.IO
open Helpers


let parse (str : string) = 
    str.Split ([|' '|], StringSplitOptions.RemoveEmptyEntries)
        |> Array.map Int32.Parse

let valid (arr : int[]) = 
    let sorted = arr |> Array.sort
    sorted.[0] + sorted.[1] > sorted.[2]

[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\..\input.txt")

    let rowValid = 
        input
        |> Seq.map parse
        |> Seq.where valid
    Console.WriteLine ("Row Triangles: " + string (rowValid |> Seq.length))

    let colValid = 
        input
        |> Seq.map parse
        |> Seq.collect id
        |> Seq.mapi (fun i x -> (i, x))
        |> Seq.groupBy (fun (i, x) -> i % 3)
        |> Seq.collect (fun (_, e) -> e)
        |> chunkBy 3
        |> Seq.map (fun arr -> arr |> Array.map (fun (i, x) -> x))
        |> Seq.where valid
    Console.WriteLine ("Col Triangles: " + string (colValid |> Seq.length))

    Console.Read ()
