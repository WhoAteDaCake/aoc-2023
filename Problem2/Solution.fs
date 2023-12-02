module MyFSharpApp.Problem2

open System

type Color =
| Red
| Green
| Blue

let (rm, gm, bm) = (uint16)12, (uint16)13, (uint16)14 // r, g, b
let gameIdSuffix = "Game ".Length
let zero = (uint16)0

let parseSet (str: string) =
    let cleaned = str.Substring(1) in
    let [count;colorStr] = cleaned.Split(" ") |> List.ofSeq in
    let countN = UInt16.Parse count in
    let color =
        match colorStr with
        | "red" ->  Red
        | "green" -> Green
        | "blue" -> Blue
        | _ -> failwith "Invalid state"
    in
    (color, countN)

let parsePull (pull: string) =
    let rec loop (r, g, b) acc =
        match acc with
        | (Red, n)::acc -> loop (r + n, g, b) acc
        | (Green, n)::acc -> loop (r, g + n, b) acc
        | (Blue, n)::acc -> loop (r, g, b + n) acc
        | [] -> (r, g, b)
    pull.Split(",") |> List.ofSeq |> List.map parseSet |> loop (zero, zero, zero) 

let parseGame (row: string) =
    row.Split(";") |> List.ofSeq |> List.map parsePull

let rec canPlay games (r: uint16, g: uint16, b: uint16) =
    if r > rm || g > gm || b > bm then
        false
    else
        match games with
        | (r1, g2, b2)::xs -> canPlay xs (r1, g2, b2)
        | [] -> true

let solveRow1 (row:string) =
    let [gameIdStr; rest] = row.Split(":") |> List.ofSeq in
    let gameId = gameIdStr.Substring(gameIdSuffix) |> UInt16.Parse
    let pulls = parseGame rest
    match canPlay pulls (zero, zero, zero) with
    | true -> gameId
    | false -> zero

let rec findMax (r: uint16, g: uint16, b: uint16) games =
    match games with
    | (r1, g2, b2)::xs -> findMax (Math.Max(r, r1), Math.Max(g, g2), Math.Max(b, b2)) xs
    | [] -> (r, g, b)


let solveRow2 (row:string) =
    let [_; rest] = row.Split(":") |> List.ofSeq in
    let pulls = parseGame rest in
    let r, g, b = findMax  (zero, zero, zero) pulls in
    (int)r * (int)g * (int)b
    // match canPlay pulls (zero, zero, zero) with
    // | true -> gameId
    // | false -> zero

let run (ls:string list): string =
    let result = List.fold (fun sum row -> sum + (int)(solveRow2 row)) 0 ls in
    result.ToString()

let spec = ("./Problem2/input_large.txt", run)