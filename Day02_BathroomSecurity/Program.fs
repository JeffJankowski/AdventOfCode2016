// http://adventofcode.com/2016/day/2
// 12/02/2016
//

open System
open System.IO


let next ((c, r) : int * int) dir =
    match dir with
    | 'U' -> (c, max (r - 1) 0)
    | 'D' -> (c, min (r + 1) 2)
    | 'L' -> (max (c - 1) 0, r)
    | 'R' -> (min (c + 1) 2, r)
    | _ -> raise <| InvalidDataException ("Bad instruction")

let KEYPAD = array2D [| 
    [| ""; ""; "1"; ""; "" |]
    [| ""; "2";"3";"4"; "" |]
    [| "5";"6";"7";"8";"9" |]
    [| ""; "A";"B";"C"; "" |]
    [| ""; ""; "D"; ""; "" |] |]

let nextWithPad (keys : string[,]) ((c, r) : int * int) dir =
    match dir with
    | 'U' -> (c, if (r - 1 < 0) || (keys.[r-1, c] = "") then r else r - 1)
    | 'D' -> (c, if (r + 1 > 4) || (keys.[r+1, c] = "") then r else r + 1)
    | 'L' -> ((if (c - 1) < 0 || (keys.[r, c-1] = "") then c else c - 1), r)
    | 'R' -> ((if (c + 1) > 4 || (keys.[r, c+1] = "") then c else c + 1), r)
    | _ -> raise <| InvalidDataException ("Bad instruction")

let next2 loc dir = nextWithPad KEYPAD loc dir


[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\..\input.txt")

    //part 1
    let code = 
        input 
        |> Seq.scan (fun pos ln -> ln |> Seq.fold next (pos)) (1, 1)
        |> Seq.skip 1
        |> Seq.map (fun (c, r) -> string (r * 3 + c + 1))
        |> String.Concat
    Console.WriteLine("Code:     " + code)

    //part 2
    let realCode = 
        input 
        |> Seq.scan (fun pos ln -> ln |> Seq.fold next2 (pos)) (0, 2)
        |> Seq.skip 1
        |> Seq.map (fun (c, r) -> KEYPAD.[r, c])
        |> String.Concat
    Console.WriteLine("Real Code: " + realCode)

    Console.Read ()