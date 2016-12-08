// http://adventofcode.com/2016/day/8
// 12/07/2016
//

open System
open System.IO


let rect (pad : bool[][]) (w : int) (h : int) =
    [0..h-1] |> List.iter (fun y -> [0..w-1] |> List.iter (fun x -> pad.[x].[y] <- true))

let rotateRow (pad : bool[][]) (r : int) (n : int) = 
    let rev = pad |> Array.map (fun arr -> arr.[r]) |> Array.rev
    let len = Array.length rev
    let shift = n % len
    let rot = Array.concat [(Array.rev rev.[0..shift-1]); (Array.rev rev.[shift..len-1])]
    for x in [0..len-1] do
        pad.[x].[r] <- rot.[x]

let rotateCol (pad : bool[][]) (c : int) (n : int) =
    let rev = pad.[c] |> Array.rev
    let len = Array.length rev
    let shift = n % len
    pad.[c] <- Array.concat [(Array.rev rev.[0..shift-1]); (Array.rev rev.[shift..len-1])]

let print (pad : bool[][]) = 
    let w = Array.length pad
    let h = Array.length pad.[0]
    for y in 0..h-1 do
        for x in 0..w-1 do
            printf "%s" (if pad.[x].[y] then "x" else " ")
        printfn ""


[<EntryPoint>]
let main argv = 
    let input = File.ReadLines("..\..\input.txt")
    let pad = Array.init 50 (fun _ -> Array.zeroCreate 6)

    input
    |> Seq.iter (fun instr ->
        let split = instr.Split (' ')
        match split with
        | [|"rect"; dim|] -> 
            let arr = dim.Split ('x') |> Array.map int
            rect pad arr.[0] arr.[1]
        | [|"rotate"; "row"; y; _; n|] -> rotateRow pad (int (y.Replace("y=", ""))) (int n)
        | [|"rotate"; "column"; x; _; n|] -> rotateCol pad (int (x.Replace("x=", ""))) (int n) )

    let count = pad |> Array.concat |> Array.filter id |> Array.length
    printfn "Lit pixels: %i\n" count
    print pad

    Console.Read ()
