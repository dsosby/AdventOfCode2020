module Day2

open Utilities

// Sample passwords
let samples = @"1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"
// Correct ones are lines 1 and 3

let isValidPassword (value : string) =
    let parts = value.Split [| ':' |]
    let rule = parts.[0]
    let password = parts.[1]
    let ruleparts = rule.Split [| '-'; ' ' |]
    let minCount = int ruleparts.[0]
    let maxCount = int ruleparts.[1]
    let letter = ruleparts.[2].[0]

    let letterCount = password |> countIf ((=) letter)
    letterCount >= minCount && letterCount <= maxCount

let solvePart1 (values : string) =
    // this would be fun with fparsec or something
    values.Split [| '\n' |]
    |> countIf isValidPassword
    |> Some

let isValidPasswordToo (value : string) =
    let parts = value.Split [| ':' |]
    let rule = parts.[0]
    let password = parts.[1]
    let ruleparts = rule.Split [| '-'; ' ' |]
    let indexone = int ruleparts.[0]
    let indextwo = int ruleparts.[1]
    let letter = ruleparts.[2].[0]
    let letterone = password.[indexone] // I didn't strip the leading space, so get 1-based index for free
    let lettertwo = password.[indextwo]
    (letterone = letter) <> (lettertwo = letter)

let solvePart2 (values : string) =
    values.Split [| '\n' |]
    |> countIf isValidPasswordToo
    |> Some

