module MyFSharpApp.Problem5

open System
open System.Text.RegularExpressions

type mapping = (uint64 * uint64 * uint64) list

let one = (uint64)1

let between s n e =
    s <= n & n <= e
    
let parseNums (str: string) =
    Regex.Matches(str, "\d+") |> Seq.map (fun c -> UInt64.Parse c.Value) |> List.ofSeq

let parseRow xs =
    let ([dest; source; len]) = xs in
    // Convert it to range
    (dest, source, source + len - one)

let rec findNumRows (rows: string list) =
    let rec loop (acc: string list) (rows: string list) =
        match rows with
        | ""::xs | ([] as xs) ->
            let rows = acc |> List.map (fun x -> x |> parseNums |> parseRow)  |> List.rev in
            (xs, List.sortBy (fun (_, source, _) -> source) rows)
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

let mapToRange (range: mapping) (value: uint64)  =
    let found = List.tryFind (fun (_, s, e) -> between s value e) range in
    match found with
    | None -> value
    | Some (d, s, _) -> value - s + d

let solve1 (ls: string list): string =
    let (seeds, lookups) = splitParts ls in
    let mapped = List.fold (fun values lookup -> List.map (mapToRange lookup) values) seeds lookups in
    (List.min mapped).ToString()


let moveRange ((s, e): uint64 * uint64) (offset: uint64) (negative: bool) =
    if negative then
        (s - offset, e - offset)
    else
        (s + offset, e + offset )

let overlap ((o, s, e): uint64 * uint64 * uint64) ((rS, rE): uint64 * uint64) =
    // No overlap
    if s > rE || e < rS then
        None
    else
        let offset, negative = (uint64)(Math.Abs(o - s)), o < s
        match rS < s, rE > e with
        // Range fits perfectly
        | false, false -> Some ((moveRange (rS, rE) offset negative), [])
        //
    

let solve2 (ls: string list): string =
    let (seeds, lookups) = splitParts ls in
    let ranges = seeds |> List.chunkBySize 2 |> List.map (fun range -> (range[0], (range[0] + range[1] - one))) in
    ""
    // let fullSeeds = List.fold (fun acc (range: uint64 list) -> [range[0]..(range[0] + range[1] - one)] @ acc ) [] ranges in
    // let mapped = List.fold (fun values lookup -> List.map (mapToRange lookup) values) fullSeeds lookups in
    // (List.min mapped).ToString()

// [10 - 20]
// [5 - 15] [16 - 30]

let spec = ("input.txt", solve2)


