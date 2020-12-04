module Utilities

let countIf pred seq =
    let oneIfTrue = function
        | c when (pred c) -> 1
        | _ -> 0
    seq
    |> Seq.sumBy oneIfTrue

let charIs (c : char) = (=) c
