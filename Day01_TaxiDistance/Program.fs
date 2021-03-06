﻿// http://adventofcode.com/2016/day/1
// 12/02/2016
//

open System
open System.IO
open System.Collections.Generic
open Helpers


//N, E, S, W
let compass = [| (0, 1); (1, 0); (0, -1); (-1, 0) |]

let move ((x, y), dir) (c : string) = 
    let newDir = modulo (dir + (if c.[0] = 'R' then 1 else -1)) 4
    let dist = Int32.Parse (c.Substring (1))
    let (mx, my) = (fst compass.[newDir] * dist, snd compass.[newDir] * dist)
    (x+mx, y+my), newDir    

let interp (x1,y1) (x2, y2) = 
    if (x1 <> x2) then
        {x1..(sign (x2-x1))..x2} |> Seq.map (fun x -> (x, y1))
    else
        {y1..(sign (y2-y1))..y2} |> Seq.map (fun y -> (x1, y))

let pickDup (p1, p2) (hash : HashSet<int*int>) =
    interp p1 p2
    |> Seq.skip 1 
    |> Seq.tryFind (fun p -> not (hash.Add (p)))


[<EntryPoint>]
let main argv = 
    let input = File.ReadAllText ("..\..\input.txt")
    let dirs = input.Split ([| ", " |], StringSplitOptions.RemoveEmptyEntries)
    
    //part 1
    let ((x1, y1), _) = dirs |> Seq.fold move ((0,0), 0)
    Console.WriteLine ( "Total Distance:   " + string (abs x1 + abs y1))

    //part 2
    let hash = new HashSet<int*int> ()
    let locs = dirs |> Seq.scan move ((0,0), 0) |> Seq.map fst
    let (x2, y2) = locs |> Seq.pairwise |> Seq.pick (fun ps -> pickDup ps hash)
    Console.WriteLine ( "Revisit Distance: " + string (abs x2 + abs y2))

    Console.Read()
