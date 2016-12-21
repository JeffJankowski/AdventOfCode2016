// http://adventofcode.com/2016/day/21
// 12/20/2016

open System
open System.IO
open Helpers


let swap_p (x:int) (y:int) (chs:char[]) = 
    let tmp = chs.[x]
    chs.[x] <- chs.[y]
    chs.[y] <- tmp
    chs

let swap_l (a:char) (b:char) (chs:char[]) = 
    swap_p (Array.IndexOf (chs, a)) (Array.IndexOf (chs, b)) chs

let rotateN (right:bool) (i:int) (chs:char[]) = 
    let n = i % chs.Length
    if n = 0 then chs else
        if right then Array.append chs.[chs.Length-n..] chs.[..chs.Length-n-1]
        else Array.append chs.[n..] chs.[..n-1]

let rotate (a:char) (chs:char[]) =
    let i = Array.IndexOf (chs, a)
    rotateN true (if i >= 4 then i + 2 else i + 1) chs

let unrotate (a:char) (chs:char[]) = 
    [0..chs.Length-1]
    |> List.map (fun i -> rotateN false i chs)
    |> List.find (fun ncharr -> chs = (rotate a ncharr))

let reverse (x:int) (y:int) (chs:char[]) =
    Array.concat [(if x = 0 then [||] else chs.[..x-1]); 
        Array.rev chs.[x..y]; 
        (if y = (chs.Length-1) then [||] else chs.[y+1..])]

let move (x:int) (y:int) (chs:char[]) =
    if x <= y then Array.concat [chs.[..x-1]; chs.[x+1..y]; chs.[x..x]; chs.[y+1..]]
    else Array.concat [chs.[..y-1]; chs.[x..x]; chs.[y..x-1]; chs.[x+1..]]

let scramble (un:bool) (seed:string) instructions = 
    instructions
    |> if un then Array.rev else Array.map id
    |> Array.fold (fun scr ins -> 
            match ins with
            | Regex @"swap position (\d) with position (\d)" [x;y] -> swap_p (int x) (int y) scr
            | Regex @"swap letter ([a-z]) with letter ([a-z])" [a;b] -> swap_l (char a) (char b) scr
            | Regex @"rotate (left|right) (\d) steps?" [dir;n] -> 
                rotateN (if un then dir = "left" else dir = "right") (int n) scr
            | Regex @"rotate based on position of letter ([a-z])" [a] -> 
                if un then unrotate (char a) scr else rotate (char a) scr
            | Regex @"reverse positions (\d) through (\d)" [x;y] -> reverse (int x) (int y) scr
            | Regex @"move position (\d) to position (\d)" [x;y] -> 
                if un then move (int y) (int x) scr else move (int x) (int y) scr
            ) (seed.ToCharArray())
    |> String.Concat

[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\\..\\input.txt")
    printfn "'abcdefgh' scrambled:    %s" (scramble false "abcdefgh" input)
    printfn "'fbgdceah' unscrambled:  %s" (scramble true "fbgdceah" input)

    Console.Read ()