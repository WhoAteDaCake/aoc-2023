module MyFSharpApp.Problem3

open System

let isSymbol (c: char) = not ((Char.IsDigit c) || c = '.')

let rec parseNumber (acc: int) (digits: char list) =
    match digits with
    | [] -> acc
    | x :: xs -> parseNumber (acc * 10 + (int) (x - '0')) xs

let mutable globalId = 0

let parser (row: string) =
    let arr = Array.create (row.Length) (-1, 0)

    let fillArr (startPos: int) (endPos: int) (value: int) =
        globalId <- globalId + 1;
        for i = startPos to endPos do
            arr.[i] <- (value, globalId)

    let rec loop (startPos: int) (curPos: int) (symbols: int list) (acc: char list) =
        if curPos = row.Length then
            (arr, symbols)
        else
            match acc with
            | [] ->
                match row[curPos] with
                | c when Char.IsDigit c -> loop curPos (curPos + 1) symbols (c :: acc)
                | '.' -> loop curPos (curPos + 1) symbols []
                // Symbol here
                | _ -> loop curPos (curPos + 1) (curPos :: symbols) []
            | digits ->
                match row[curPos] with
                | c when Char.IsDigit c -> loop startPos (curPos + 1) symbols (c :: acc)
                | _ ->
                    let value = parseNumber 0 (List.rev digits) in
                    let _ = fillArr startPos (curPos - 1) value in
                    loop curPos curPos symbols []

    loop 0 0 [] []

let rec buildMatrix idx (symbols: (int * int) list) (matrix: (int * int) array array) (rows: string list) =
    match rows with
    | [] -> (symbols, matrix)
    | row :: rows ->
        let (values, rowSymbols) = parser row in
        let _ = matrix[idx] <- values in
        let rowSymbols = List.map (fun i -> (idx, i)) rowSymbols in
        buildMatrix (idx + 1) (symbols @ rowSymbols) matrix rows

let matrixLookup (matrix: (int * int) array array) (maxX: int) (maxY: int) =
    let lookup (y, x) =
        if x < 0 || x >= maxX || y < 0 || y >= maxY then
            None
        else
            let (value, id) = matrix[y][x] in if value = -1 then None else Some (value, id) in

    lookup

let run (ls: string list) : string =
    let maxY = ls.Length in
    let maxX = ls.[0].Length in
    let (symbols, matrix) = buildMatrix 0 [] (Array.create ls.Length [||]) ls in
    let lookup = matrixLookup matrix maxX maxY in
    let found =
        List.fold
            (fun acc (y: int, x: int) ->
                let coords = [ (y - 1, x); (y + 1, x); (y, x - 1); (y, x + 1); (y - 1, x - 1); (y - 1, x + 1); (y + 1, x - 1); (y + 1, x + 1) ] in
                let points = List.map lookup coords in
                acc @ points)
            []
            symbols in
    let uniques =
        List.fold (fun acc value ->
            match value with
            | None -> acc
            | Some (value, id) -> Map.add id value acc) (Map []) found in
    let sum = Map.fold (fun acc _ value -> acc + value) 0 uniques in
    sum.ToString()

let spec = ("./Problem3/input_large.txt", run)
