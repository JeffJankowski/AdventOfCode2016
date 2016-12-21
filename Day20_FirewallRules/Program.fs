// http://adventofcode.com/2016/day/20
// 12/20/2016

open System
open System.IO
open System.Collections.Generic


let rec min (n: uint32) (ranges: (uint32*uint32) list) = 
    let incl = ranges |> List.tryFind (fun (l,h) -> n >= l && n <= h)
    if incl.IsNone then n else 
        min (snd incl.Value + 1u) ranges

let rec combine (ranges: (uint32*uint32) list) (grouped: (uint32*uint32) list) = 
    if ranges.Length = 0 then grouped else
        let (s0,e0) = List.head ranges
        let overlaps = List.filter (fun (s,e) -> s0 <= e && s <= e0) ranges
        if Seq.length overlaps = 1 then combine (List.tail ranges) ((s0,e0)::grouped) else
        let set = new HashSet<uint32*uint32> (overlaps)
        let except = ranges |> List.filter (fun r -> not (set.Contains (r)))
        let cmb = List.fold (fun (grpS: uint32,grpE: uint32) (s:uint32:uint32,e) -> 
            (Math.Min(s,grpS), Math.Max(e,grpE))) (s0,e0) overlaps
        combine (cmb::except) grouped


[<EntryPoint>]
let main argv = 
    let ranges = 
        File.ReadAllLines ("..\\..\\input.txt")
        |> Array.map (fun (s: string) -> 
            let arr = s.Split ('-') |> Array.map uint32
            (arr.[0], arr.[1]) ) 
        |> Array.toList
    
    printfn "Min valid IP: %i" (min 0u ranges)

    let groups = combine ranges [] |> List.sortBy fst
    let invalid = List.fold (fun acc (s1,e1) -> acc + (e1-s1+1u)) 0u groups
    printfn "Valid IPs:    %i" (4294967295u - invalid + 1u)
        
    Console.Read ()
