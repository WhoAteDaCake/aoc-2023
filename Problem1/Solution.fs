module MyFSharpApp.Problem1

open System
open System.Text.RegularExpressions

let file = "./Problem1/input_small.txt"

let rx = Regex(".*(\d).*(\d)", RegexOptions.Compiled)

let solve_row row =
    let result = rx.Match row in
    let parts = [for g in result.Groups -> g.Value] in
    String.Concat("", parts) |> Int32.Parse
    
let run (ls:string list) =
   "hello"
 
let spec = ("./Problem1/input_small.txt", run)