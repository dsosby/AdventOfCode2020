namespace AdventOfCode2020

open System

module Solutions =

    let private commutativePairs (nums : int []) =
        let maxidx = nums.Length - 1
        seq {
            for i in 0..maxidx do
                for j in i..maxidx do
                    (nums.[i], nums.[j])
        }

    let private commutativeTriplets (nums : int []) =
        let maxidx = nums.Length - 1
        seq {
            for i in 0..maxidx do
                for j in i..maxidx do
                    for k in j..maxidx do
                        (nums.[i], nums.[j], nums.[k])
        }

    let private solve (input : string) =
        input.Split [|'\n'|]
        |> Array.map int
        |> commutativePairs
        |> Seq.tryFind (fun (lhv, rhv) -> lhv + rhv = 2020)
        |> Option.map (fun (lhv, rhv) -> lhv * rhv)

    let private solvePart2 (input : string) =
        input.Split [|'\n'|]
        |> Array.map int
        |> commutativeTriplets
        |> Seq.tryFind (fun (lhv, rhv, rhvp) -> lhv + rhv + rhvp = 2020)
        |> Option.map (fun (lhv, rhv, rhvp) -> lhv * rhv * rhvp)

    let Day1 input =
        match input with
        | (null|"") -> "Bad travel expenses"
        | input ->
            match (solve input) with
            | None -> "Nothing found"
            | Some solution -> solution.ToString()

    let Day1Part2 input = 
        match input with
        | (null|"") -> "Bad travel expenses"
        | input ->
            match (solvePart2 input) with
            | None -> "Nothing found"
            | Some solution -> solution.ToString()
