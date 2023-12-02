module MyFSharpApp.Problem1

open System
open System.Text.RegularExpressions

let file = "./Problem1/input_small.txt"

let rx = Regex(".*(\d).*(\d)", RegexOptions.Compiled)

let solve_row row =
    let result = rx.Match row in
    let parts = [for g in result.Groups -> g.Value] in
    String.Concat("", parts) |> Int32.Parse
    

let run (ls: string list) =
    ""
    
type Solution() =
    interface Solution.T with
        member this.file = "./Problem1/input_small.txt"
        member this.run (ls:string list) =
            "hello"
