module MyFSharpApp.Problem1

open System

let possible = ["one"; "two"; "three"; "four"; "five";"six"; "seven"; "eight"; "nine"]
let longest = possible |> List.map String.length |> List.sortDescending |> List.head

let rec findWord (idx: int32) (str: string) (words: string list) =
    match words with
    | x::xs -> if str.StartsWith(x) then Some(idx,x) else findWord (idx + 1) str (xs)
    | [] -> None

let (|MatchesWord|_|) (row: char list) =
   let takeL = Math.Min(longest, row.Length)
   let str = new string(List.take takeL row |> Array.ofList) in
   findWord 1 str possible

let rec finder acc (row: char list) =
    match row with
    | MatchesWord (value, word) ->
        finder (value::acc) (List.skip 1 row)
    | x::xs -> if Char.IsDigit x then finder ((Int32.Parse(x.ToString()))::acc) xs else finder acc xs
    | [] -> List.rev acc

let solveRow row =
    let numbers = finder [] (List.ofSeq row) in
    // let numRow = String.Join(",", Array.ofList numbers ) in
    // Reversed, since the finder prepends instead of push
    let first = List.head numbers in
    let last = List.last numbers in
    // let _ = Console.WriteLine($"{row} -> {numRow} [{first * 10 + last}]") in
    first * 10 + last
    
let run (ls:string list): string =
    let result = List.fold (fun sum row -> sum + (solveRow row)) 0 ls in
    result.ToString()

let spec = ("./Problem1/input_large.txt", run)