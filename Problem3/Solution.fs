module MyFSharpApp.Problem3

open System

let isSymbol (c: char) =
    not ((Char.IsDigit c) || c = '.')

let rec parseNumber (acc: int) (digits: char list) =
    match digits with
    | [] -> acc
    | x::xs -> parseNumber (acc * 10 + (int)(x - '0')) xs

let parser (row: string) =
    let arr = Array.create (row.Length) -1
    let fillArr (startPos: int) (endPos: int) value =
        for i = startPos to endPos do
            arr.[i] <- value
    let rec loop (startPos: int) (curPos: int) (symbols: int list) (acc: char list) =
        if curPos = row.Length then
            (arr, symbols)
        else
            match acc with
            | [] ->
                match row[curPos] with
                | c when Char.IsDigit c -> loop curPos (curPos + 1) symbols (c::acc)
                | '.' -> loop curPos (curPos + 1) symbols []
                // Symbol here
                | _ -> loop curPos (curPos + 1) (curPos::symbols) []
            | digits ->
                   match row[curPos] with
                    | c when Char.IsDigit c -> loop startPos (curPos + 1) symbols (c::acc)
                    | _ ->
                        let value = parseNumber 0 (List.rev digits) in
                        let _ = fillArr startPos (curPos - 1) value in
                        loop curPos (curPos + 1) symbols [] 
    loop 0 0 [] []
       
let rec buildMatrix idx (symbols: (int * int) list) (matrix: int array array) (rows: string list) =
    match rows with
    | [] -> (symbols, matrix)
    | row::rows ->
        let (values, rowSymbols) = parser row in
        let _ = matrix[idx] <- values in
        let rowSymbols = List.map (fun i -> (idx, i)) rowSymbols in
        buildMatrix (idx + 1) (symbols @ rowSymbols) matrix rows

let run (ls:string list): string =
    let maxY = ls.Length in
    let maxX = ls[0].Length in
    let (symbols, matrix) = buildMatrix 0 [] (Array.create ls.Length [||]) ls in
    ""

let spec = ("./Problem3/input_small.txt", run)