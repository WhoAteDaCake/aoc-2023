module MyFSharpApp.Problem4

open System
open System.Text.RegularExpressions
open System.Linq

let intersect (xs:'a seq) (ys: 'a seq) = xs.Intersect(ys)

let parseNums (str: string) =
    Regex.Matches(str, "\d+") |> Seq.map (fun c -> Int32.Parse c.Value)

let getMatching (row: string) =    
    let [winning;mine] = (row.Split(": ")[1]).Split(" | ") |> List.ofSeq in
    let winning, mine = parseNums winning, parseNums mine in
    intersect winning mine |> Seq.length

let solveRow (row: string) =
    let matching = (getMatching row) - 1 in
    if matching < 0 then 0.0 else Math.Pow((double)2, matching)
    
let solve1 (ls:string list): string =
    let result = List.fold (fun sum row -> sum + (int)(solveRow row)) 0 ls in
    result.ToString()
    
let fillScores (ls: int list) =
    let max = List.length ls in
    let limit = max - 1 in 
    let arr = Array.create max 1 in
    let rec loop idx acc =
        match acc with
        | [] -> arr
        | x::xs ->
           for i in idx .. Math.Min(idx + x - 1, limit) do
                arr[i] <- arr[i] + arr[idx - 1]
           loop (idx + 1) xs
    in
    loop 1 ls

let solve2 (ls: string list): string =
    let result = List.map getMatching ls in
    let items = fillScores result in
    let all = Array.fold (fun total n -> total + n) 0 items in
    all.ToString()

let spec = ("./Problem4/input_large.txt", solve2)

