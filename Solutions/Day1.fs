namespace AdventOfCode2020

open System

module Solutions =

    let solve (input : string) =
        let nums = input.Split "\n" |> Array.map int
        let targetCondition = (fun (lhv, rhv) -> lhv + rhv = 2020)
        let maxidx = nums.Length - 1
        let sums = seq {
            for i in 0..maxidx do
                for j in i..maxidx do
                    (nums.[i], nums.[j])
        }
        let pair = Seq.tryFind targetCondition sums
        Option.map (fun (lhv, rhv) -> lhv * rhv) pair

    let Day1 input =
        match input with
        | (null|"") -> "Bad travel expenses"
        | input ->
            match (solve input) with
            | None -> "Nothing found"
            | Some solution -> solution.ToString()
