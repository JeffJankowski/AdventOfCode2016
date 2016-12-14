// http://adventofcode.com/2016/day/11
// 12/12/2016


// INPUT:
//  The first floor contains a polonium generator, a thulium generator, a thulium-compatible microchip, 
//    a promethium generator, a ruthenium generator, a ruthenium-compatible microchip, a cobalt generator, 
//    and a cobalt-compatible microchip.
//  The second floor contains a polonium-compatible microchip and a promethium-compatible microchip.
//  The third floor contains nothing relevant.
//  The fourth floor contains nothing relevant.


//  F0   .   PoG .   PmG TmM TmG RuM RuG CoM CoG   <-- elevator start
//  F1   PoM .   PmM .   .   .   .   .   .   .
//  F2   .   .   .   .   .   .   .   .   .   .
//  F3   .   .   .   .   .   .   .   .   .   .     <-- all objects end here 


open System
open System.IO
open System.Collections.Generic
open Helpers


type Type = Microchip | Generator
type Element = Cobalt | Polonium | Promethium | Ruthenium | Thulium | Elerium | Dilithium
type Object = Option<Element * Type>


let N = None
let S x = Some x

let row n (state: Object[,]) = state.[n..n,*] |> Seq.cast<Object>

let equals (a: Object[,]) (b: Object[,]) = 
    (Seq.cast<Object> a)
    |> Seq.zip (Seq.cast<Object> b)
    |> Seq.map (fun (aa,bb) -> aa=bb)
    |> Seq.fold (&&) true

type ObjectComparer() = 
  interface IEqualityComparer<Object[,]*int> with
    member x.Equals(a, b) = equals (fst a) (fst b) && (snd a) = (snd b)
    member x.GetHashCode((os,f)) = 
        os |> Seq.cast<Object> |> Seq.fold 
            (fun h o -> h * 31 + (hash o)) 19 
        |> (+) (hash f)

let dirs n = 
    match n with
    | 0 -> [1] 
    | 3 -> [2]
    | _ -> [n-1; n+1]

let isDone (state: Object[,], currFloor: int) =
    if currFloor <> 3 then false else row 3 state |> Seq.forall (fun o -> o.IsSome)

let isValid n (state: Object[,]) = 
    let r = row n state
    let objs = r |> Seq.choose id
    let hasGen = objs |> Seq.exists (fun (_,t) -> t = Generator)
    if not hasGen then true else
        Seq.groupBy (fun (e,_) -> e) objs
        |> Seq.forall (fun (_, os) -> 
            match Seq.toList os with
            | [(_,Generator)] | [(_,Generator);(_,Microchip)] | [(_,Microchip);(_,Generator)] -> true
            | _ -> false )


let step (state: Object[,], currFloor: int) (set: HashSet<Object[,]*int>) = 
    let goodStates =
        [1..2] 
        |> List.collect (fun i -> comb i (row currFloor state |> Seq.choose id |> Seq.toList))
        |> List.map (fun l -> 
            match l with
            | [o1; o2] -> (S o1),(S o2)
            | [o1] -> (S o1),N )
        |> List.filter (fun (o1, o2) -> 
            match (o1, o2) with
            | Some (e1,t1), Some (e2,t2) when t1 <> t2 && e1 <> e2 -> false
            | _ -> true )
        |> List.collect (fun objs -> (dirs currFloor) |> List.map (fun f -> objs,f))
        |> List.map (fun ((o1,o2), f) ->
            let newState = Array2D.copy state
            [o1;o2]
            |> List.map (fun o -> 
                if o.IsNone then None else 
                    (row currFloor state ) |> Seq.findIndex (fun x -> x = o) |> Some)
            |> List.iter (fun i -> 
                if i.IsSome then
                    newState.[f,i.Value] <- state.[currFloor, i.Value]
                    newState.[currFloor, i.Value] <- None )
            newState,f )
        |> List.filter (fun (state,floor) -> 
            (isValid floor state) && (isValid currFloor state) && not (set.Contains((state,floor))) )
    
    for sf in goodStates do
        set.Add(sf) |> ignore

    goodStates


[<EntryPoint>]
let main argv = 
    let initState : Object[,] = 
        [[N; S (Polonium,Generator); N; S (Promethium,Generator); S (Thulium,Microchip); S (Thulium,Generator); 
            S (Ruthenium,Microchip); S (Ruthenium,Generator); S (Cobalt,Microchip); S (Cobalt,Generator);
            S (Elerium,Generator); S (Elerium,Microchip); S (Dilithium,Generator); S (Dilithium,Microchip)];
        [S (Polonium,Microchip); N; S (Promethium,Microchip); N; N; N; N; N; N; N; N; N; N; N]; 
        [N; N; N; N; N; N; N; N; N; N; N; N; N; N];
        [N; N; N; N; N; N; N; N; N; N; N; N; N; N]] |> array2D

    let set = new HashSet<Object[,]*int> (new ObjectComparer())
    set.Add (initState, 0) |> ignore
    let min =
        Seq.initInfinite id
        |> Seq.scan (fun states _ -> states |> List.collect (fun sf -> step sf set)) [(initState, 0)]
        |> Seq.findIndex (fun states -> 
            printfn "states = %i\t\t set (%i)" (states.Length) (set.Count)
            if (states.Length = 0) then true else
                states |> Seq.exists isDone)

    printfn "Min steps: %i" min

    Console.Read ()