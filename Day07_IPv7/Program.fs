// http://adventofcode.com/2016/day/7
// 12/07/2016
//

open System
open System.IO

let isABBA (seg : string) =
    seg.ToCharArray ()
    |> Seq.windowed 4
    |> Seq.exists (fun chunk -> 
        chunk.[0] = chunk.[3] && chunk.[1] = chunk.[2] && chunk.[0] <> chunk.[1])

let allABA (seg : string) = 
    seg.ToCharArray ()
    |> Seq.windowed 3
    |> Seq.filter (fun chunk -> chunk.[0] = chunk.[2] && chunk.[0] <> chunk.[1])
    |> Seq.map String.Concat

let toBAB (aba : string) = String.Concat [|aba.[1]; aba.[0]; aba.[1]|]

let split (ip : string) = Array.foldBack (fun x (l,r) -> x::r, l) (ip.Split ([|'[';']'|])) ([],[])
    
let tls (ip : string) =
    let super, hyper = split ip
    (List.exists isABBA super) && not (List.exists isABBA hyper)

let ssl (ip : string) = 
    let super, hyper = split ip
    super 
    |> Seq.collect allABA
    |> Seq.exists (fun aba -> hyper |> Seq.exists (fun seg -> seg.Contains(toBAB aba)))
        

[<EntryPoint>]
let main argv = 
    let input = File.ReadLines("..\..\input.txt")
    
    let validTLS = input |> Seq.filter tls
    printfn "Valid TLS: %i" (validTLS |> Seq.length)

    let validSSL = input |> Seq.filter ssl
    printfn "Valid SSL: %i" (validSSL |> Seq.length)

    Console.Read ()
