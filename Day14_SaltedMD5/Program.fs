// http://adventofcode.com/2016/day/14
// 12/13/2016

open System
open System.IO
open System.Text
open System.Security.Cryptography
open System.Collections.Generic

let dict = new Dictionary<int,string>()

let SALT = "ihaygndm"
let md5 = MD5.Create ()
let hash i n = 
    if dict.ContainsKey (i) then dict.[i] else
    let h = 
        [1..n]
        |> List.fold (fun (s: string) _ -> 
            s
            |> Encoding.ASCII.GetBytes
            |> md5.ComputeHash
            |> Seq.map (fun c -> c.ToString("x2"))
            |> Seq.reduce (+) ) (SALT + (string i))
    dict.[i] <- h
    h

let goodkey (idx: int) (ch: char) n = 
    [idx+1..idx+1000]
    |> List.exists (fun i -> (hash i n) |> Seq.windowed 5 |> Seq.exists (fun arr -> 
        Array.forall (fun c -> ch = c) arr))

let sixtyFour n = 
    Seq.initInfinite id
    |> Seq.filter (fun i ->
        let trip = (hash i n) |> Seq.windowed 3 |> Seq.tryFind (fun arr -> arr.[0] = arr.[1] && arr.[1] = arr.[2])
        match trip with
        | Some [|c;_;_|] -> goodkey i c n
        | None -> false )
    |> Seq.skip 63
    |> Seq.head


[<EntryPoint>]
let main argv = 

    printfn "64th good key index:  %i" (sixtyFour 1)
    dict.Clear ()
    printfn "64th stretched index: %i" (sixtyFour 2017)

    md5.Dispose()
    Console.Read ()