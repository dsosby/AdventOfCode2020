module Day1

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

let solvePart1 (input : string) =
    input.Split [|'\n'|]
    |> Array.map int
    |> commutativePairs
    |> Seq.tryFind (fun (lhv, rhv) -> lhv + rhv = 2020)
    |> Option.map (fun (lhv, rhv) -> lhv * rhv)

let solvePart2 (input : string) =
    input.Split [|'\n'|]
    |> Array.map int
    |> commutativeTriplets
    |> Seq.tryFind (fun (lhv, rhv, rhvp) -> lhv + rhv + rhvp = 2020)
    |> Option.map (fun (lhv, rhv, rhvp) -> lhv * rhv * rhvp)
