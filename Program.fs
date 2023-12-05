open System
open System.IO
open MyFSharpApp;


type Solver = (string * string list -> string)

let (file, run) = Problem5.spec

let content =
    let workingDir = Environment.CurrentDirectory in
    let projectDir = Directory.GetParent(workingDir).Parent.Parent.FullName in
    let textFile = Path.Combine(projectDir, file) in
    File.ReadLines(textFile) |> List.ofSeq
let stopWatch = System.Diagnostics.Stopwatch.StartNew()
let result = run content
stopWatch.Stop()
printfn $"Elapsed %s{stopWatch.Elapsed.ToString()}\nResult: {result}"