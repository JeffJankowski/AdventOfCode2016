// http://adventofcode.com/2016/day/4
// 12/05/2016
//

open System
open System.IO
open Helpers


type Room = { Name: string; Sector: int; Checksum: string; }

[<Literal>]
let PATTERN = @"([a-z-]+)-([0-9]{0,3})\[([a-z]{5})\]"
let parse (str : string) = 
    match str with
    | Regex PATTERN [name; sector; chksum] -> {Name = name; Sector = int sector; Checksum = chksum}
    | _ -> raise <| InvalidDataException ("Bad room")


let isValid (room : Room) = 
    let check = 
        room.Name.Replace ("-", "")
        |> Seq.groupBy id
        |> Seq.sortBy (fun (c, s) -> -(Seq.length s), c)
        |> Seq.map (fun (c, _) -> c)
        |> Seq.take 5
        |> String.Concat
    check = room.Checksum

[<Literal>]
let A_CHAR = 97
let shift (name : string) (n: int) =
    name
    |> Seq.map (fun c -> 
        match c with
        | '-' -> ' '
        | _ -> char (((int c - A_CHAR + n) % 26) + A_CHAR) )
    |> String.Concat


[<EntryPoint>]
let main argv = 
    let input = File.ReadAllLines ("..\..\input.txt")
    let rooms = input |> Seq.map parse
    
    let sectorSum =
        rooms
        |> Seq.where isValid
        |> Seq.sumBy (fun r -> r.Sector)
    Console.WriteLine("Sector Sum: " + string sectorSum)
    
    let (npName, npSector) = 
        rooms 
        |> Seq.map (fun r -> (shift r.Name r.Sector), r.Sector)
        |> Seq.find (fun (decr, sector) -> decr.Contains ("north"))
    Console.WriteLine(String.Format ("[{0}] {1}", npSector, npName))

    Console.Read ()