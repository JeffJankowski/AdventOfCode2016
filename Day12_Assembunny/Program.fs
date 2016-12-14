// http://adventofcode.com/2016/day/12
// 12/13/2016

open System
open System.IO
open Helpers


let a = ref 0
let b = ref 0
let c = ref 1
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

[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\..\input.txt")

    let mutable i = 0
    while i < input.Length do
        match input.[i] with
        | Regex @"cpy (.+) ([a-z])" [v; r] -> (reg r) := (regval v)
        | Regex @"inc ([a-z])" [r] -> (reg r) := (reg r).Value + 1
        | Regex @"dec ([a-z])" [r] -> (reg r) := (reg r).Value - 1
        | Regex @"jnz (.+) (.+)" [v; w] -> if (regval v) <> 0 then i <- i + (regval w) - 1
        i<-i+1

    printfn "a=%i  b=%i  c=%i  d=%i" a.Value b.Value c.Value d.Value
    Console.Read ()
