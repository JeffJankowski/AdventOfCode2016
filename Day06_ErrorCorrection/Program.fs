// http://adventofcode.com/2016/day/6
// 12/05/2016
//

open System
open System.IO

let correct (grps : seq<seq<char>>) func = 
    grps
    |> Seq.map (fun grp ->
        grp
        |> Seq.groupBy id
        |> Seq.sortBy func
        |> Seq.head
        |> fst )
    |> String.Concat


[<EntryPoint>]
let main argv = 
    let input = File.ReadLines("..\..\input.txt")
    let grps = 
        input
        |> Seq.map (fun s -> s.ToCharArray())
        |> Seq.concat
        |> Seq.mapi (fun i c -> i,c)
        |> Seq.groupBy (fun (i,_) -> i % 8)
        |> Seq.sortBy fst
        |> Seq.map (fun (_,chs) -> Seq.map snd chs)

    let corrected = correct grps (fun (_,chs) -> -Seq.length chs)
    Console.WriteLine("Corrected: " + corrected)
    
    let decoded = correct grps (fun (_,chs) -> Seq.length chs)
    Console.WriteLine("Decoded:   " + decoded)

    Console.Read ()