namespace AdventOfCode2020

open System

module Solutions =

    let private commutativePairs (nums : int [])=
        let maxidx = nums.Length - 1
        seq {
            for i in 0..maxidx do
                for j in i..maxidx do
                    (nums.[i], nums.[j])
        }

    let private solve (input : string) =
        input.Split [|'\n'|]
        |> Array.map int
        |> commutativePairs
        |> Seq.tryFind (fun (lhv, rhv) -> lhv + rhv = 2020)
        |> Option.map (fun (lhv, rhv) -> lhv * rhv)

    let Day1 input =
        match input with
        | (null|"") -> "Bad travel expenses"
        | input ->
            match (solve input) with
            | None -> "Nothing found"
            | Some solution -> solution.ToString()
