// http://adventofcode.com/2016/day/22
// 12/23/2016


// NOTE: Solved part 2 by hand. Was gonna be another humongous BFS, so it was much easier
//       just to print out the grid as summarized by the problem and solve by hand. The mapped
//       grid was incredibly simplistic anyway.

open System
open System.IO
open System.Text
open Helpers

let good ((Au,_): (int*int)) ((_,Ba): (int*int)) = Au <> 0 && Ba >= Au

[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\\..\\input.txt") |> Array.skip 2
    let dic = 
        input
        |> Array.map (fun s ->
            match s with
            | Regex @"/dev/grid/node-x(\d+)-y(\d+)\s+\d+T\s+(\d+)T\s+(\d+)T\s+\d+%" [x;y;u;a] ->
                (int x, int y), (int u, int a) )
        |> dict

    let viable = 
        comb 2 (List.ofSeq dic.Keys)
        |> List.collect (fun keys ->
            match keys with
            | [a;b] -> [(a,b);(b,a)])
        |> List.filter (fun (a,b) -> good dic.[a] dic.[b])
    printfn "Viable: %i" (viable.Length)

//    let sb = new StringBuilder()
//    for y in [0..24] do
//        for x in [0..36] do
//            let u,a = dic.[x,y]
//            sb.AppendFormat("{0}/{1} ",u.ToString("000"), (u+a).ToString("000")) |> ignore
//        sb.AppendLine() |> ignore
//    File.WriteAllText("..\\..\\results.txt", sb.ToString())

//    let sb = new StringBuilder()
//    for y in [0..24] do
//        for x in [0..36] do
//            let s = 
//                match (x,y) with
//                | (0,0) -> "[.]"
//                | (36,0) -> " G "
//                | (x,y) -> 
//                    let u,a = dic.[x,y]
//                    if u = 0 then " _ " else if (u+a) > 100 then " # " else " . "
//            sb.Append (s) |> ignore
//        sb.AppendLine() |> ignore
//    File.WriteAllText("..\\..\\map.txt", sb.ToString())

    
    //print viable adjacent set
    viable
    |> List.filter (fun ((ax,ay),(bx,by)) ->
        let dx = Math.Abs(ax-bx)
        let dy = Math.Abs(ay-by)
        dx <= 1 && dy <= 1 && (dx = 0 || dy = 0))
    |> List.iter (fun (a,b) -> printfn "%O  -->  %O" a b)

    printfn "done"
    Console.Read ()