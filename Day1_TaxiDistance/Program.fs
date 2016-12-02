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
    let dist = Int32.Parse (c.Substring(1))
    let (mx, my) = (fst compass.[newDir] * dist, snd compass.[newDir] * dist)
    (x+mx, y+my), newDir    


[<EntryPoint>]
let main argv = 
    let input = File.ReadAllText ("..\..\input.txt")
    let dirs = input.Split ([| ", " |], StringSplitOptions.RemoveEmptyEntries)
    
    let ((x, y), _) = dirs |> Seq.fold move ((0,0), 0)
    Console.WriteLine ( "Total Distance: " + string (abs x + abs y))


    Console.Read()
