// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.

open System
open System.IO
open Helpers


let swap_p (x:int) (y:int) (charr:char[]) = 
    let tmp = charr.[x]
    charr.[x] <- charr.[y]
    charr.[y] <- tmp
    charr

let swap_l (a:char) (b:char) (charr:char[]) = 
    let x = Array.IndexOf (charr, a)
    let y = Array.IndexOf (charr, b)
    swap_p x y charr

let rotateN (right:bool) (i:int) (charr:char[]) = 
    let n = i % charr.Length
    if n = 0 then charr else
        match right with
        | false -> Array.append charr.[n..charr.Length-1] charr.[0..n-1]
        | true -> Array.append charr.[charr.Length-n..charr.Length-1] charr.[0..charr.Length-n-1]

let rotate (a:char) (charr:char[]) =
    let i = Array.IndexOf (charr, a)
    let n = if i >= 4 then i + 2 else i + 1
    rotateN true n charr

let unrotate (a:char) (charr:char[]) = 
    [0..charr.Length-1]
    |> List.map (fun i -> rotateN false i charr)
    |> List.find (fun ncharr -> charr = (rotate a ncharr))

let reverse (x:int) (y:int) (charr:char[]) =
    Array.concat [(if x = 0 then [||] else charr.[0..x-1]); 
        Array.rev charr.[x..y]; 
        (if y = (charr.Length-1) then [||] else charr.[y+1..charr.Length-1])]

let move (x:int) (y:int) (charr:char[]) =
    if x <= y then
        Array.concat [charr.[0..x-1]; charr.[x+1..y]; charr.[x..x]; charr.[y+1..charr.Length-1]]
    else
        Array.concat [charr.[0..y-1]; charr.[x..x]; charr.[y..x-1]; charr.[x+1..charr.Length-1]]

let scramble (seed:string) instructions = 
    instructions
    |> Array.fold (fun scr ins -> 
            match ins with
            | Regex @"swap position (\d) with position (\d)" [x;y] -> swap_p (int x) (int y) scr
            | Regex @"swap letter ([a-z]) with letter ([a-z])" [a;b] -> swap_l (char a) (char b) scr
            | Regex @"rotate (left|right) (\d) steps?" [dir;n] -> rotateN (dir = "right") (int n) scr
            | Regex @"rotate based on position of letter ([a-z])" [a] -> rotate (char a) scr
            | Regex @"reverse positions (\d) through (\d)" [x;y] -> reverse (int x) (int y) scr
            | Regex @"move position (\d) to position (\d)" [x;y] -> move (int x) (int y) scr
            ) (seed.ToCharArray())
    |> String.Concat

let unscramble (seed:string) instructions = 
    instructions
    |> Array.rev
    |> Array.fold (fun scr ins -> 
            match ins with
            | Regex @"swap position (\d) with position (\d)" [x;y] -> swap_p (int x) (int y) scr
            | Regex @"swap letter ([a-z]) with letter ([a-z])" [a;b] -> swap_l (char a) (char b) scr
            | Regex @"rotate (left|right) (\d) steps?" [dir;n] -> rotateN (dir = "left") (int n) scr
            | Regex @"rotate based on position of letter ([a-z])" [a] -> unrotate (char a) scr
            | Regex @"reverse positions (\d) through (\d)" [x;y] -> reverse (int x) (int y) scr
            | Regex @"move position (\d) to position (\d)" [x;y] -> move (int y) (int x) scr
            ) (seed.ToCharArray())
    |> String.Concat

[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\\..\\input.txt")

    let scrambled = scramble "abcdefgh" input
    printfn "'abcdefgh' scrambled:    %s" scrambled

    let unscrambled = unscramble "fbgdceah" input
    printfn "'fbgdceah' unscrambled:  %s" unscrambled

    Console.Read ()