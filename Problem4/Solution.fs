module MyFSharpApp.Problem4

open System
open System.Text.RegularExpressions
open System.Linq

// let nRe =/

let intersect (xs:'a seq) (ys: 'a seq) = xs.Intersect(ys)

let parseNums (str: string) =
    Regex.Matches(str, "\d+") |> Seq.map (fun c -> Int32.Parse c.Value)
    
let solveRow (row: string) =
    let [winning;mine] = (row.Split(": ")[1]).Split(" | ") |> List.ofSeq in
    let winning, mine = parseNums winning, parseNums mine in
    let matching = intersect winning mine |> Seq.length in
    let matching = matching - 1 in
    Math.Pow((double)2, matching)
    
let solve1 (ls:string list): string =
    let result = List.fold (fun sum row -> sum + (int)(solveRow row)) 0 ls in
    result.ToString()
    
let solve2 (ls: string list): string
    let result = List.map solveRow ls in
    ""

let spec = ("./Problem4/input_large.txt", solve2)

