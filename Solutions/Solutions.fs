module AdventOfCode2020.Solutions

open Microsoft.FSharp.Reflection

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

let solve day part input =
    match input with
    | (null|"") -> "This should be an HTTP 400"
    | input ->
        let solver = findSolver day part
        match solver with
        | Some solver ->
            solver input
            |> unbox<obj option>
            |> function
                | Some solution -> solution.ToString()
                | None -> "This should be an HTTP 400 maybe? (input was likely bad, or the solution sucks)"
        | None -> "This should be an HTTP 404"
