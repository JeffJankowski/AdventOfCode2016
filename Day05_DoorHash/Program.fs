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
    |> Seq.map (fun c -> c.ToString("x2"))
    |> Seq.reduce (+)

let replace (curr : string) (i : int) (c : char) =
    let arr = curr.ToCharArray ()
    arr.[i] <- c
    String.Concat arr

let print (map : char[]) = 
    map
    |> Array.fold (fun acc x -> 
        match x with
        | '\000' -> acc + "_"
        | ch -> acc + (string ch)) ""


[<EntryPoint>]
let main argv = 
    use md5 = MD5.Create ()

    let hashes = 
        Seq.initInfinite id
        |> Seq.map (fun i -> md5hash md5 (INPUT + (string i)))
        |> Seq.where (fun hash -> hash.StartsWith("00000"))

    Console.Write("Password:    ________")
    let pass = 
        hashes
        |> Seq.take 8
        |> Seq.map (fun hash -> hash.[5])
        |> Seq.fold (fun acc x -> 
            let incomp = replace acc (acc.IndexOf('_')) x
            Console.Write("\rPassword:    " + incomp)
            incomp) "________"

    Console.Write("\nBetter Pass: ________")
    let passMap = Array.zeroCreate<char> 8
    hashes
    |> Seq.map (fun hash -> Int32.TryParse (hash.[5].ToString ()), hash.[6])
    |> Seq.takeWhile (fun _ -> 
        passMap |> Array.exists (fun c -> c = '\000' ))
    |> Seq.iter (fun ((parsed, pos), ch) ->   
        if (parsed && pos < 8 && passMap.[pos] = '\000') then 
            passMap.[pos] <- ch
            Console.Write("\rBetter Pass: " + print passMap) )

    Console.Read ()