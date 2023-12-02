open System
open System.IO
open MyFSharpApp;

type Solver = (string * string list -> string)

let (file, run) = Problem1.spec

let content =
    let workingDir = Environment.CurrentDirectory in
    let projectDir = Directory.GetParent(workingDir).Parent.Parent.FullName in
    let textFile = Path.Combine(projectDir, file) in
    File.ReadLines(textFile) |> List.ofSeq
let result = run content
Console.Out.WriteLine result