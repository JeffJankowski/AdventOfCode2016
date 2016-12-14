// http://adventofcode.com/2016/day/13
// 12/13/2016


// ## ##            ###     ##### #####    #  ##  # #
//#O###### ## ####   # ##    #  # #  # ## #### ##
// oo ## #  # ## # # ######  # ##  # ####  # #######
//##oooo###      #  #  #  #### ###  #  # # ##  #
// ####o####  ### # ## # #   #  # # ## ##      #  ###
//     oo###### ###  ###  ## #  ###  ## #  ## ####
//  ## #o ##  #       ### #  ###      # #####  # #
//## #  ooooo ## ## #     #    # ## # ###  # # ####
// ## ######o# # ##   ### ##   ## #  #  ## ##   #####
//# ##    ##oo## # ###  # ### # ## #  #  ## #     #
//## #  # ###o#  #   # ##   #  # #  #       ##  # ##
// ######  # o# ###### ##### #  ###  ## ### ####   #
//   #     ##o# ##  #   ## # ## ####  # ###    #   #
//## # ##   #oooooo #      #     # ###   ######## ###
//#  # ### #######o######## ## # ### ##   ##  # # ##
//#  #  ## #   ###o##ooo# ## #  #     ##    # ##
//### # ## #     #oooo#oo# ## # #### # # ##  # #####
//# ###   ### ## # ### #o## ###   ##  ## # #      ##
//#     #  ## #  ##  ## o #     #  ## #  #  ##  # ##
//#  #####    #   ### # o ## ##### #  #  ## #####  ##
//##    # ## ### #  # ##o# ###  #  ## #####
// ###  ####  ##  # # ##oo#ooooo#   # #  ##### ## ##
//## #    # #   #  ##  ##ooo###o## ##  #  ## # ## #
//   ###  ## ##  # ### # #### #o## ### #     #    # #
//#### ##  #######  #  #     ##ooooo#  ###### ##  #
//#     ###  #      #  ### # # ### o#     # ## ### #
//# ## #  ## #  ######## ### ### ##o##  # #  ### #  #
//# ##  #   ####    #       #   # #o##### # #   ###
//# # #   #    #  # # ##  # ###  ##o##   ## ### ####
//  ## ############ # #####    # # oooo# ##      # #
// # # #  ##  #    ##  #    ## # #  ##o#    ## # ##
//  ## #    # ## # # # # ### #  ######o# ### # ## #
//# #  ## #  # # # ##  ##  ## #     #oo### ##   # ###
//  # # ## #   # ## #   # # ######  #o#  ## # # # # #
//  #  # #  ###   # ## ## ##  ## #  #oooo # #  ##
//#  #  ### # # # #### ##  #     ##  ## o #  # ####
//## ## ###  ##  #   #     #  ######  ##o####   #####
// #     # # # #  ## # ## #####  # ## ##o## #    #
//# #### ##  #  # #  ## # ##  #  #### ##o  ##### #
//##    # # ###   #   ##      ###Xo##oooo# ##  ####
// #  # # # #### ### # ##### #   #oooo## #       ####
//##### #    ###  ##      ##  ##   ### # # ##  # ## #
// #   ### #  # #   ##  # ### # ## # ##  ## ###   #
// # # # # #  ## ## #####  #  ## # ## #   # # #   ##
// ### ##  ##  #### ##     ##  ##   #### ## # ## # #
//   ## #   ###  #     ##   ### #     ## ## # ##  ##
//    #### #   # #  ## ### #    ##  #      ##  ## # #
//# #   ##  ##  ######  ##  ### ######## # ### #  ##
//#   # ### # #     # #   # ###     ## ###  #  ##  ##
// ####  #  ######  ## ##    #####          #   ###
//#      ###  ## #   ######   ## #####  ###### #   ##



// First part solved through printing out the maze...and solving it.

open System
open System.IO
open System.Collections.Generic


let wall (x,y) = 
    let mutable i = x*x + 3*x + 2*x*y + y + y*y + 1352
    i <- i - ((i >>> 1) &&& 0x55555555)
    i <- (i &&& 0x33333333) + ((i >>> 2) &&& 0x33333333)
    ((((i + (i >>> 4)) &&& 0x0F0F0F0F) * 0x01010101) >>> 24) % 2 = 1

let print (maze: bool[,]) = 
    for y in [0..Array2D.length2 maze - 1] do
        for x in [0..Array2D.length1 maze - 1] do
            match x,y with
            | 1,1 -> "O"
            | 31,39 -> "X"
            | x,y -> if maze.[x,y] then "#" else " "
            |> printf "%s"
        printfn ""

let pos (x,y) (maze: bool[,]) (set: HashSet<int*int>) = 
    List.append
        (if (x = 0) then [(x+1,y)] else [(x-1,y); (x+1,y)])
        (if (y = 0) then [(x,y+1)] else [(x,y-1); (x,y+1)])
    |> List.filter (fun (x,y) -> not maze.[x,y] && not (set.Contains (x,y)))


[<EntryPoint>]
let main argv = 
    let maze = Array2D.init 50 50 (fun x y -> wall (x,y))
    print maze

    let set = new HashSet<int*int> ()
    set.Add (1,1) |> ignore
    Seq.unfold (fun acc -> 
        if snd (List.head acc) = 50 then None else
            let options = List.collect (fun (p,n) -> 
                let good = pos p maze set
                List.iter (fun loc -> set.Add (loc) |> ignore) good
                good
                |> List.map (fun p -> p,n+1) ) acc
            Some (options,options)
        ) [(1,1),0]
    |> Seq.toList |> ignore //force the lazy seq

    printfn "\nDistinct locs: %i" set.Count

    Console.Read ()
