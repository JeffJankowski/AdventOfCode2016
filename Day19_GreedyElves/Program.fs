// http://adventofcode.com/2016/day/19
// 12/19/2016

open System
open System.IO
open System.Collections.Generic

let rec ssanta (elves: (int*int)[]) = 
    if elves.Length = 1 then elves.[0] else
        for i in 0..elves.Length-1 do
            if (snd elves.[i] > 0) then 
                elves.[i] <- (fst elves.[i], (snd elves.[i]) + (snd elves.[(i+1) % elves.Length]))
                elves.[(i+1) % elves.Length] <- (fst elves.[(i+1) % elves.Length], 0)
        ssanta (Array.filter (fun (_,n) -> n > 0) elves)

let rec santa (elves: (int*int)[]) = 
    if elves.Length = 1 then elves.[0] else
        let mid = elves.Length / 2;
        for i in 0..mid-1 do
            let x = (i*2 + (elves.Length - i)/2) % elves.Length
            elves.[i] <- fst elves.[i], (snd elves.[i]) + (snd elves.[x])
            elves.[x] <- (fst elves.[x], 0)
        let arr =  
            Array.append elves.[mid..elves.Length-1] elves.[0..mid-1]
            |> Array.filter (fun (_,i) -> i > 0)
        santa arr


[<EntryPoint>]
let main argv =
    let elves = Array.init 3004953 (fun i -> (i, 1))
    let elves_clone = Array.copy elves
    printfn "Shitty santa winner: %i" (ssanta elves |> fst |> (+) 1)
    printfn "Circle santa winner: %i" (santa elves_clone |> fst |> (+) 1)

    Console.Read ()