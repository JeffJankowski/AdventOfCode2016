// http://adventofcode.com/2016/day/10
// 12/12/2016
//

open System.IO;
open System;
open Helpers;
open System.Collections.Generic;


type Bot = {
    id: int
    mutable data: (Option<int> * Option<int>) }

type Output = {
    id: int
    mutable data: Option<int>}

type Node = 
    | BotNode of Bot 
    | OutNode of Output


[<EntryPoint>]
let main argv = 
    let input = System.IO.File.ReadAllLines ("..\..\input.txt")
    let (nodes, values) = 
        input
        |> Array.fold (fun (graph, vals) s ->
            match s with
            | Regex @"bot (\d+) gives low to (bot|output) (\d+) and high to (bot|output) (\d+)" 
                [b; lt; l; ht; h] -> ((int b,lt,int l,ht,int h)::graph, vals)
            | Regex @"value (\d+) goes to bot (\d+)" [v; b] -> (graph,(int v, int b)::vals)) ([],[])

    let bots:Bot[] = Array.init 300 (fun i -> {id = i; data = (None, None)})
    let outs:Output[] = Array.init 50 (fun i -> {id = i; data = None})
    let adj = new Dictionary<Bot, Node*Node>()
    
    //build graph
    nodes
    |> List.iter (fun (b,lt,l,ht,h) -> 
        let low = if lt = "bot" then BotNode(bots.[l]) else OutNode(outs.[l])
        let high = if ht = "bot" then BotNode(bots.[h]) else OutNode(outs.[h])
        adj.Add(bots.[b], (low,high)))


    let rec run (value : int) (node : Node) = 
        match node with
        | BotNode bot ->
            match bot.data with
            | None, None -> 
                bot.data <- (Some value, None)
                [(bot.id, bot.data)]
            | Some x, None -> 
                let (low, high) = (Math.Min (x, value)), (Math.Max (x, value))
                bot.data <- (None, None)
                [[bot.id, (Some low, Some high)]; run low (fst adj.[bot]); run high (snd adj.[bot])] |> List.concat
        | OutNode output ->
            output.data <- Some value
            []

    //run sim and collect comparison values
    let (cell, _) = 
        values 
        |> List.collect (fun (v, b) -> run v (BotNode(bots.[b])) )
        |> List.find (fun (_, data) -> 
            match data with
            | Some 17, Some 61 -> true 
            | _, _ -> false)

    printfn "Bot compare ID: %i" cell
    printfn "Output product: %i" (outs.[0].data.Value * outs.[1].data.Value * outs.[2].data.Value)

    Console.Read ()
    