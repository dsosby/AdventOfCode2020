module Day6

let clean (str: string) = str.Replace(" ", "").Replace("\n", "")
let split (str: string) = str.Split [|'\n'|]

[<Solution(part = 1)>]
let sumOfGroupCounts (input: string) =
    input.Trim().Split("\n\n")
    |> Seq.map clean
    |> Seq.map Set.ofSeq
    |> Seq.map Set.count
    |> Seq.sum

[<Solution(part = 2)>]
let sumOfGroupWithSetUnion (input: string) =
    input.Trim().Split("\n\n")
    |> Seq.map (split >> Seq.map Set.ofSeq)
    |> Seq.map Set.intersectMany
    |> Seq.map Set.count
    |> Seq.sum

[<Literal>]
let sample = @"abc

a
b
c

ab
ac

a
a
a
a

b"
