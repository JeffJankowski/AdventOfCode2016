// http://adventofcode.com/2016/day/17
// 12/17/2016

open System
open System.IO
open System.Security.Cryptography
open System.Text

type Dir = Up = 0 | Down = 1 | Left = 2 | Right = 3
let compass = 
    [|(Dir.Up, (0,-1), "U"); (Dir.Down, (0,1), "D"); 
    (Dir.Left, (-1,0), "L"); (Dir.Right, (1,0), "R")|]

let md5 = MD5.Create ()
let md5hash (input : string) = 
    input
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("x2"))
    |> Seq.reduce (+)

let avail (input: string) (x:int, y:int) = 
    (md5hash input).Substring(0, 4)
    |> Seq.mapi (fun i ch -> 
        let (dir,_,_) = compass.[i];
        if (Char.IsDigit(ch) || ch = 'a') then None else (Some dir) )
    |> Seq.choose id
    |> Seq.filter (fun d ->
        match d with
        | Dir.Up -> y > 0
        | Dir.Down -> y < 3
        | Dir.Left -> x > 0
        | Dir.Right -> x < 3 )

let move ((x,y): int*int) (dir: Dir) = 
    let (_,(offx,offy),_) = compass.[(int dir)]
    (x + offx, y + offy)

let rec step ((x,y): int*int) (n: int) (input: string) = 
    if ((x,y) = (3,3)) then [(n, input)] else
        let dirs = avail input (x,y)
        if Seq.isEmpty dirs then [] else
            dirs
            |> Seq.collect (fun d -> 
                let (_,_,newCh) = compass.[int d]
                step (move (x,y) d) (n+1) (input+newCh) )
            |> Seq.toList


[<EntryPoint>]
let main argv =
    let allpaths = (step (0,0) 0 "yjjvjgan")
    printfn "Min path dirs:   %s" ((snd (List.minBy fst allpaths)).Substring(8))
    printfn "Max path length: %i" (fst (List.maxBy fst allpaths))

    Console.Read ()
