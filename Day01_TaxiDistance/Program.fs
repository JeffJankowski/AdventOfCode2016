// http://adventofcode.com/2016/day/1
// 12/02/2016
//

open System
open System.IO
open System.Collections.Generic


// real modulus that workds with negatives instead of .NET's crap
let modulo n m = ((n % m) + m) % m

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


[<EntryPoint>]
let main argv = 
    let input = File.ReadAllText ("..\..\input.txt")
    let dirs = input.Split ([| ", " |], StringSplitOptions.RemoveEmptyEntries)
    
    //part 1
    let ((x1, y1), _) = dirs |> Seq.fold move ((0,0), 0)
    Console.WriteLine ( "Total Distance: \t" + string (abs x1 + abs y1))

    //part 2
    let locs = dirs |> Seq.scan move ((0,0), 0) |> Seq.map fst
    let points = locs |> Seq.pairwise |> Seq.collect (fun (p1,p2) -> interp p1 p2 |> Seq.skip 1)
    let hash = new HashSet<int*int> ()
    let (x2, y2) = points |> Seq.pick (fun p -> if hash.Add (p) then None else Some(p) )
    Console.WriteLine ( "Revisit Distance:\t" + string (abs x2 + abs y2))

    Console.Read()
