// http://adventofcode.com/2016/day/18
// 12/19/2016

open System
open System.IO


let trap lcr = 
    match lcr with
    | (true, true, false)
    | (false, true, true)
    | (true, false, false)
    | (false, false, true) -> true
    | _ -> false

let generate line = 
    (false::line) @ ([false])
    |> Seq.windowed 3
    |> Seq.map (fun arr -> trap (arr.[0], arr.[1], arr.[2]))
    |> Seq.toList


[<EntryPoint>]
let main argv = 
    let input = 
        ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"
        |> Seq.map (fun c -> c = '^')
        |> Seq.toList
    let safe = 
        input::
            (Seq.unfold (fun (str, n) -> 
                if (n >= 40) then None else 
                    let newrow = generate str
                    Some (newrow, (newrow,n+1)) ) (input, 1)
            |> Seq.toList)
        |> List.collect id
        |> List.filter (id >> not) 
        |> List.length

    printfn "Safe tiles: %i" safe
    Console.Read ()
