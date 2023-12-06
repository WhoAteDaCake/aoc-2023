module MyFSharpApp.Problem5

open System
open System.Text.RegularExpressions

module List =
    let mapFindFirst f ls =
        let rec loop ls =
            match ls with
            | [] -> None
            | x::xs ->
                match f x with
                | None -> loop xs
                | Some a -> Some a
        in
        loop ls

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


let moveRange (offset: uint64) (negative: bool) ((s, e): uint64 * uint64)  =
    if negative then
        (s - offset, e - offset)
    else
        (s + offset, e + offset )

// 53 - 60 <- Mapping
// 57 - 69 <-

let overlap ((rS, rE): uint64 * uint64) ((o, s, e): uint64 * uint64 * uint64) =
    // No overlap
    // [..range.] [...mapp.]
    // [..mapp...] [range]
    if s > rE || e < rS then
        None
    else
        let offset, negative = (uint64)(Math.Abs(o - s)), o < s in
        let move = moveRange offset negative in
        match rS < s, rE > e with
        // Range fits perfectly
        | false, false -> Some (move (rS, rE), [])
        //    [....] <- Range
        // [...] <- Mapping
        | false, true ->
            // TODO: check if -1 here is needed
            let leftover = (e + one, rE) in
            Some (move (rS, e), [leftover])
        // [...] <- Range
        //    [...] <- Mapping
        | true, false ->
            // TODO: check if -1 here is needed
            let leftover = (rS, rE - e) in
            Some (move (s, rE), [leftover])

// [0] = {Tuple<ulong, ulong>} (81, 94)
// [1] = {Tuple<ulong, ulong>} (57, 69)

let applyMappings (mapping: mapping) (ranges: (uint64 * uint64) list) =
    let ranges = List.sortBy fst ranges in
    let rec loop (ranges: (uint64 * uint64) list) (acc: (uint64 * uint64) list) =
        match ranges with
        | [] -> acc
        | r::rs ->
            match List.mapFindFirst (overlap r) mapping with
            | None -> loop rs (r::acc)
            | Some (nRange, leftovers) -> loop (leftovers @ rs) (nRange::acc)
    in
    loop ranges []

// 79 - 92
// 55 - 67

// Ranges
// 98 - 99
// 50 - 97 

let rangesToString ranges =
    let ls = List.map (fun (s, e) -> $"({s} - {e})") ranges in
    String.Join(", ", ls)
    
let lookupToString lookups =
    let ls = List.map (fun (o, s, e) -> $"({s} - {e}, {o})") lookups in
    String.Join(", ", ls)

let solve2 (ls: string list): string =
    let (seeds, lookups) = splitParts ls in
    let ranges = seeds |> List.chunkBySize 2 |> List.map (fun range -> (range[0], (range[0] + range[1] - one))) in
    let output = List.fold (fun acc lookup ->
        let _ = Console.Out.WriteLine ((rangesToString acc) + "|" + lookupToString lookup) in
        applyMappings lookup acc ) ranges lookups in
    (List.min output).ToString()
    // let test1 = applyMappings lookups[0] ranges in
    // ""
    // let fullSeeds = List.fold (fun acc (range: uint64 list) -> [range[0]..(range[0] + range[1] - one)] @ acc ) [] ranges in
    // let mapped = List.fold (fun values lookup -> List.map (mapToRange lookup) values) fullSeeds lookups in
    // (List.min mapped).ToString()

// [10 - 20]
// [5 - 15] [16 - 30]

let spec = ("input.txt", solve2)


