open System
open System.IO
open MyFSharpApp;

let solver = Problem1.Solution() :> Solution.T
let fullFile = Path.Combine 
let content =
    let workingDir = Environment.CurrentDirectory in
    let projectDir = Directory.GetParent(workingDir).Parent.Parent.FullName in
    let textFile = Path.Combine(projectDir, solver.file) in
    File.ReadLines(textFile) |> List.ofSeq
let result = solver.run content
Console.Out.WriteLine result