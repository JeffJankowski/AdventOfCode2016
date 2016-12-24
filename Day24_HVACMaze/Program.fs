// http://adventofcode.com/2016/day/24
// 12/23/2016

open System
open System.IO
open System.Collections.Generic
open Helpers


let solve (start: int*int) (finish: int*int) (map: bool[][]) = 
    let q = new Queue<(int*int)*int>([(start,0)]);
    let set = new HashSet<int*int> ()
    let mutable min = None
    while min.IsNone do
        let ((x,y),n) = q.Dequeue ()
        if (x,y) = finish then min <- Some n else
            [(x+1,y); (x-1,y); (x,y-1); (x,y+1)]
            |> List.filter (fun (x0,y0) -> 
                x0 >= 0 && y0 >= 0 && y0 < map.Length && x0 < map.[0].Length && 
                map.[y0].[x0] && (set.Contains (x0,y0) |> not))
            |> List.iter (fun p -> 
                set.Add (p) |> ignore
                q.Enqueue (p,(n+1)))
    min.Value

[<EntryPoint>]
let main argv = 
    let targets = Array.zeroCreate<int*int> 8
    let map = 
        File.ReadAllLines ("..\\..\\input.txt")
        |> Array.mapi (fun y s ->
            s.ToCharArray()
            |> Array.mapi (fun x c ->
                match c with
                | '#' -> false
                | '.' -> true
                | p -> targets.[(int (p.ToString()))] <- (x,y); true))

    let dists =
        comb 2 [0..7]
        |> List.map (fun l -> (l.[0], l.[1]))
        |> List.map (fun (s,e) -> (s,e), (solve targets.[s] targets.[e] map ))
        |> dict

    let min perms = 
        perms
        |> List.map (fun l ->
            List.pairwise l
            |> List.map (fun (i,j) -> if i < j then dists.[i,j] else dists.[j,i])
            |> List.sum )
        |> List.min
        
    let paths = permute [1..7] |> List.map (fun l -> 0::l)
    printfn "Min path steps:  %i" (min paths)
    printfn "Min cycle steps: %i" (min (List.map (fun l -> l @ [0]) paths))

    Console.Read ()