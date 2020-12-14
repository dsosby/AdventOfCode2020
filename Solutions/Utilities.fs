[<AutoOpen>]
module Utilities

open System
open System.Collections

type SolutionAttribute (part : int) =
    inherit Attribute()
    member this.Part = part

let countIf pred seq =
    let oneIfTrue = function
        | c when (pred c) -> 1
        | _ -> 0
    seq
    |> Seq.sumBy oneIfTrue

let charIs (c : char) = (=) c

let splitLines (str: string) = str.Split [| '\n' |]

let immutableSetBit (bitarray: BitArray) idx v =
    // TODO Find a better immutable bit array
    let cloned = BitArray(bitarray)
    cloned.Set(idx, v)
    cloned
