module Day2

// Sample passwords
let samples = @"1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc"
// Correct ones are lines 1 and 3

let countLetters (letter : char) (str : string) =
    str
    |> Seq.sumBy (fun c ->
        match c with
        | c when c = letter -> 1
        | _ -> 0
    )

let isValidPassword (value : string) =
    let parts = value.Split [| ':' |]
    match parts with
    | [| rule;password |] ->
        let ruleparts = rule.Split [| '-'; ' ' |]
        let mincount = int ruleparts.[0]
        let maxcount = int ruleparts.[1]
        let letter = ruleparts.[2].[0]
        let lettercount = countLetters letter password
        match lettercount with
        | c when c >= mincount && c <= maxcount -> true
        | _ -> false
    | _ -> false

let solvePart1 (values : string) =
    // this would be fun with fparsec or something
    values.Split [| '\n' |]
    |> Seq.map isValidPassword
    |> Seq.filter (fun x -> true = x) // Is there a better way to express a truth predicate?
    |> Seq.length
    |> Some

