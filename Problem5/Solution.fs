module MyFSharpApp.Problem5

open System
open System.Text.RegularExpressions

let parseNums (str: string) =
    Regex.Matches(str, "\d+") |> Seq.map (fun c -> Int32.Parse c.Value) |> List.ofSeq

let parseRow xs =
    let ([dest; source; len]) = xs in
    (dest, source, len)

let rec findNumRows (rows: string list) =
    let rec loop (acc: string list) (rows: string list) =
        match rows with
        | ""::xs | ([] as xs) -> (xs, acc |> List.map (fun x -> x |> parseNums |> parseRow)  |> List.rev)
        | x::xs -> loop (x::acc) xs
    in
    loop [] (List.tail rows)

let splitAllRows (rows: string list) =
    let rec loop (acc) xs =
        match xs with
        | [] -> List.rev acc
        | xs ->
            let rest, entries = findNumRows xs in
            loop (entries::acc) rest
    in
    loop [] rows

let splitParts (ls: string list) =
    let first::rest = ls in
    let seeds = parseNums first in
    let [
        seedToSoil
        soilToFert
        fertToWater
        waterToLight
        lightToTemp
        tempToHumid
        humidToLoc
    ] = splitAllRows (List.tail rest)
    ""

let solve1 (ls: string list): string =
    let parts = splitParts ls in
    ""

let spec = ("./Problem5/input_small.txt", solve1)


