module MyFSharpApp.Problem5

open System
open System.Text.RegularExpressions

let between s n e =
    s <= n & n <= e
    
let parseNums (str: string) =
    Regex.Matches(str, "\d+") |> Seq.map (fun c -> UInt64.Parse c.Value) |> List.ofSeq

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
    let lookups = splitAllRows (List.tail rest) in
    (seeds, lookups)

let mapToRange (range: (uint64 * uint64 * uint64) list) (value: uint64)  =
    let one = (uint64)1 in
    let found = List.tryFind (fun (_, s, l) -> between s value (s + l - one)) range in
    match found with
    | None -> value
    | Some (d, s, _) -> value - s + d

let solve1 (ls: string list): string =
    let (seeds, lookups) = splitParts ls in
    let mapped = List.fold (fun values lookup -> List.map (mapToRange lookup) values) seeds lookups in
    (List.min mapped).ToString()

let spec = ("./Problem5/input_large.txt", solve1)


