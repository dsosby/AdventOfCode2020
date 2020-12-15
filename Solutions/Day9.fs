module Day9

let complementSet nums =
    // Need to use F# 5.0 -- can upgrade if I get rid of the Azure Functions lib
    // It would have reverse slicing using ^
    let sumLast = (+) <| Array.last nums
    let length = nums |> Array.length

    nums.[..length-2]
    |> Seq.map sumLast
    |> Set.ofSeq

let parseInput (input: string) =
    input.Trim()
    |> splitLines
    |> Array.map int64

[<Solution(part = 1)>]
let firstInvalidRow (input: string) =
    // Given last n numbers, a new number is considered
    // valid iff a sum of 2 of the n numbers equals the new number
    // Solution:
    //  Start at nth
    //  Look back nth
    //  Calculate all sums of previous int, store in Set
    //  Given a new number, n+1, look at sets calculated for prior window
    let windowSize = 25
    let inputs = input |> parseInput
    let sets =
        inputs
        |> Seq.mapi (fun idx i ->
            (i, complementSet inputs.[idx - windowSize .. idx]))
        |> Seq.cache

    let firstInvalidRow = 
        sets
        |> Seq.windowed (windowSize + 1)
        |> Seq.tryFind (fun sets ->
            // TODO Seq.last is probably slow
            let checkSets = sets |> Seq.take windowSize |> Seq.map snd
            let toCheck = sets |> Seq.last |> fst
            let containsSum = Set.contains toCheck

            checkSets
            |> Seq.exists containsSum
            |> not)

    firstInvalidRow
    |> Option.map (Seq.last >> fst)

// Part 2

let sumMinAndMax nums = (nums |> Seq.min) + (nums |> Seq.max)

let findWindowSummingTo (nums: int64 []) (value: int64) =
    // Have two cursors (indexes) and a running sum
    // while sum < value, add a new item
    // if sum > value, subtract an item
    // if sum = value, return slice between
    // Mutable is bad, so do it recursively
    let rec run i j acc =
        if acc = value then nums.[i..j-1]
        else if acc < value then run i (j + 1) (acc + nums.[j])
        else if acc > value then run (i + 1) j (acc - nums.[i])
        else [||]

    run 0 1 nums.[0]

[<Solution(part = 2)>]
let findTheWeakness (input: string) =
    // TODO input is parsed twice
    let inputs = input |> parseInput
    let windowFinder = findWindowSummingTo inputs

    // Start with result from Solution 1
    // Find a contiguous list of numbers that sum to above solution
    // Return the min and max of this list
    firstInvalidRow input
    |> Option.map (windowFinder >> sumMinAndMax)
