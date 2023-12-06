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

type mapping = (int64 * int64 * int64) list

let one = (int64)1

let between s n e =
    s <= n & n <= e
    
let parseNums (str: string) =
    Regex.Matches(str, "\d+") |> Seq.map (fun c -> Int64.Parse c.Value) |> List.ofSeq

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

let mapToRange (range: mapping) (value: int64)  =
    let found = List.tryFind (fun (_, s, e) -> between s value e) range in
    match found with
    | None -> value
    | Some (d, s, _) -> value - s + d

let solve1 (ls: string list): string =
    let (seeds, lookups) = splitParts ls in
    let mapped = List.fold (fun values lookup -> List.map (mapToRange lookup) values) seeds lookups in
    (List.min mapped).ToString()


let moveRange (offset: int64) ((s, e): int64 * int64)  =
     (s + offset, e + offset )

// 53 - 60 <- Mapping
// 57 - 69 <-

let overlap ((rS, rE): int64 * int64) ((o, s, e): int64 * int64 * int64) =
    // No overlap
    // [..range.] [...mapp.]
    // [..mapp...] [range]
    if s > rE || e < rS then
        None
    else
        let offset = o - s in
        let move = moveRange offset in
        match rS < s, rE > e with
        | true, true -> Some (move (rS, rE), [])
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

let applyMappings (mapping: mapping) (ranges: (int64 * int64) list) =
    let ranges = List.sortBy fst ranges in
    let rec loop (ranges: (int64 * int64) list) (acc: (int64 * int64) list) =
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
    let ls = List.map (fun (o, s, e) -> $"({s} - {e}, {o - s})") lookups in
    String.Join(", ", ls)

let solve2 (ls: string list): string =
    let (seeds, lookups) = splitParts ls in
    let ranges = seeds |> List.chunkBySize 2 |> List.map (fun range -> (range[0], (range[0] + range[1] - one))) in
    let output = List.fold (
        fun acc lookup ->
            let result =  applyMappings lookup acc in
            result) ranges lookups in
    let (lowest, _) = List.minBy fst output in
    lowest.ToString()


let spec = ("input_large.txt", solve2)


