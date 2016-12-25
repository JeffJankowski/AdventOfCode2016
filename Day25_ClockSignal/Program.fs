// http://adventofcode.com/2016/day/25
// 12/25/2016

open System
open System.IO
open Helpers

let a = ref 0
let b = ref 0
let c = ref 0
let d = ref 0
let ic = ref 0

let reg s =
    match s with
    | "a" -> a
    | "b" -> b
    | "c" -> c
    | "d" -> d

let regval s =
    let x = ref 0
    if Int32.TryParse (s, x) then x.Value else (reg s).Value


let run (instr: string[]) = 
    seq { 
        ic := 0
        while ic.Value < instr.Length do
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
            | Regex @"out (.+)" [v] -> yield (regval v)
            ic := ic.Value + 1 }

[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\..\input.txt")
    let min = 
        Seq.initInfinite id
        |> Seq.pick (fun i ->
            a := i
            b := 0
            c := 0
            d := 0
            let good = 
                run input
                |> Seq.take 10
                |> Seq.chunkBySize 2
                |> Seq.forall (fun chk -> chk.[0] = 0 && chk.[1] = 1)
            if good then Some i else None )
    printfn "Repeating signal when reg 'a' = %i" min

    Console.Read ()