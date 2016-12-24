// http://adventofcode.com/2016/day/23
// 12/23/2016

open System
open System.IO
open Helpers

let a = ref 0
let b = ref 0
let c = ref 0
let d = ref 0

let reg s =
    match s with
    | "a" -> a
    | "b" -> b
    | "c" -> c
    | "d" -> d

let regval s =
    let x = ref 0
    if Int32.TryParse (s, x) then x.Value else (reg s).Value

let tgl (i: int) (instr: string[]) = 
    if i >= 0 && i < instr.Length then
        instr.[i] <-
            match instr.[i] with
            | Regex @"inc (.+)" [a] -> "dec " + a
            | Regex @"dec|tgl (.+)" [a] -> "inc " + a
            | Regex @"jnz (.+) (.+)" [a; b] -> sprintf "cpy %s %s" a b
            | Regex @"cpy (.+) (.+)" [a; b] -> sprintf "jnz %s %s" a b
        

let exec (ic: ref<int>) (instr: string[]) = 
    match instr.[ic.Value] with
        | Regex @"cpy (.+) ([a-z])" [v; r] -> if Char.IsLetter(r.Chars 0) then (reg r) := (regval v)
        | Regex @"inc ([a-z])" [r] -> 
            if Char.IsLetter(r.Chars 0) then 
                let i = ic.Value
                match (sprintf "%s %s %s %s" instr.[i+1] instr.[i+2] instr.[i+3] instr.[i+4]) with
                | Regex @"dec ([a-z]) jnz ([a-z]) -2 dec ([a-z]) jnz ([a-z]) -5" [a; a1; b; b1] 
                    when a = a1 && b = b1 -> 
                        ic := ic.Value + 4
                        (reg r) := (reg r).Value + ((int (reg a).Value) * (int (reg b).Value))
                | _ -> (reg r) := (reg r).Value + 1
        | Regex @"dec ([a-z])" [r] -> if Char.IsLetter(r.Chars 0) then (reg r) := (reg r).Value - 1
        | Regex @"jnz (.+) (.+)" [v; w] -> if (regval v) <> 0 then ic := ic.Value + (regval w) - 1
        | Regex @"tgl (.+)" [v] -> tgl (ic.Value + (regval v)) instr

let run (input: string[]) = 
    let ic = ref 0
    while ic.Value < input.Length do
        exec ic input
        ic := ic.Value + 1

[<EntryPoint>]
let main argv = 
    a := 7
    run (File.ReadAllLines ("..\..\input.txt"))
    printfn "When eggs is 7:  a = %i" a.Value

    a := 12
    b := 0
    c := 0
    d := 0
    run (File.ReadAllLines ("..\..\input.txt"))
    printfn "When eggs is 12: a = %i" a.Value

    Console.Read ()