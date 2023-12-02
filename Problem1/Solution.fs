module MyFSharpApp.Problem1

open System



let rec finder acc (row: char list) =
    match row with
    | 'o'::'n'::'e'::xs -> finder (1::acc) xs
    // | one, two, three, four, five, six, seven, eight, and nine
    | x::xs -> if Char.IsDigit x then finder ((Int32.Parse x)::acc) xs else finder acc xs
    | [] -> List.rev acc

let solve_row row =
    
    // let chars = Seq.toList row in
    // let strNum =
    //     match finder chars, finder (List.rev chars) with
    //     | Some(c), Some(c2) -> new string([|c; c2|])
    //     | _ -> failwith "Impossible state"
    // strNum |> Int32.Parse
    
let run (ls:string list): string =
    let result = List.fold (fun sum row -> sum + (solve_row row)) 0 ls in
    result.ToString()
 
let spec = ("./Problem1/input_large.txt", run)