// http://adventofcode.com/2016/day/5
// 12/05/2016
//

open System
open System.IO
open System.Text
open System.Security.Cryptography


[<Literal>]
let INPUT = "abbhdwsy"

let md5hash (md5 : MD5) (input : string) = 
    input
    |> Encoding.ASCII.GetBytes
    |> md5.ComputeHash
    |> Seq.map (fun c -> c.ToString("X2"))
    |> Seq.reduce (+)


[<EntryPoint>]
let main argv = 
    use md5 = MD5.Create ()

    let hashes = 
        Seq.initInfinite id
        |> Seq.map (fun i -> md5hash md5 (INPUT + (string i)))
        |> Seq.where (fun hash -> hash.StartsWith("00000"))

    let pass = 
        hashes
        |> Seq.take 8
        |> Seq.map (fun hash -> hash.[5])
        |> String.Concat
    Console.WriteLine("Password:    " + pass)

    let passMap = Array.zeroCreate<string> 8
    hashes
    |> Seq.pick (fun hash ->
        let parsed, pos = Int32.TryParse (hash.[5].ToString ())
        if parsed && pos < 8 && String.IsNullOrEmpty (passMap.[pos]) then 
            passMap.[pos] <- hash.[6].ToString ()
        if passMap |> Array.exists (String.IsNullOrEmpty) then None else Some(passMap))
    |> ignore
    Console.WriteLine("Better Pass: " + (passMap |> String.Concat))

    Console.Read ()