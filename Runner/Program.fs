open Argu
open System
open Microsoft.FSharp.Reflection

type CliArgument =
    | Day of int
    | Part of int
    | Time
    | InputFile of path:string

    interface IArgParserTemplate with
        member s.Usage = 
            match s with
            | Day _ -> "specify a day"
            | Part _ -> "specify a part (defaults to 1)"
            | Time _ -> "output performance timing of solution"
            | InputFile _ -> "specify an input file. Defaults to Inputs/Day%d.txt"

let findSolver day part =
    let moduleName = sprintf "Day%d" day
    let assembly = typeof<Utilities.SolutionAttribute>.Assembly
    let modules =
        assembly.GetTypes()
        |> Array.filter FSharpType.IsModule

    assembly.GetType(moduleName).GetMethods()
    |> Array.tryFind (fun mi ->
        mi.CustomAttributes
        |> Seq.exists (fun attr ->
            attr.AttributeType = typeof<Utilities.SolutionAttribute> &&
            (unbox<int> attr.ConstructorArguments.[0].Value) = part))
    |> Option.map (fun mi (input : string) -> mi.Invoke(null, [| input |]))

[<EntryPoint>]
let main argv =
    let argParser = ArgumentParser.Create<CliArgument>(programName = "aoc2020")

    try
        let args = argParser.ParseCommandLine(inputs = argv, raiseOnUsage = true)
        let day = args.GetResult Day
        let part = args.GetResult (Part, defaultValue = 1)
        let solver = findSolver day part

        match solver with
        | Some solver ->
            let defaultInputFile = sprintf "Inputs/Day%d.txt" day
            let inputFile = args.GetResult (InputFile, defaultValue = defaultInputFile)
            let rawInput = System.IO.File.ReadAllText inputFile
            let saneInput = rawInput.Replace("\r", "")
            let solution = solver saneInput
            printfn "%A" solution
            // TODO Unbox to option obj and print w/o "Some ..."
            0
        | None -> eprintfn "No solution found for Day %d Part %d" day part; 1
    with e ->
        printfn "%s" e.Message; 1

