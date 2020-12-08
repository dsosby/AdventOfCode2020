module Day5

type RowGuide = Front | Back
exception BadInput

let rowGuide = function
    | ('F'|'L') -> Front
    | ('B'|'R') -> Back
    | _ -> raise BadInput

// Can't believe .net has no exponentiation for integers.. fingers crossed for no accuracy errors
let ( ^^ ) x y = int (double x ** double y)

let getPosition max idx guide =
    match guide with
    | Front -> 0
    | Back -> (2 ^^ max) >>> (idx + 1)

let findIndex (input: string) =
    let mapper = getPosition input.Length
    input.Trim()
    |> Seq.map rowGuide
    |> Seq.mapi mapper
    |> Seq.sum

let findSeat (input: string) =
    let row = findIndex input.[..6]
    let col = findIndex input.[7..]
    let cabinWidth = 8
    row, col, row * 8 + col

[<Solution(part = 1)>]
let solve (input: string) =
    input.Trim().Split("\n")
    |> Seq.map findSeat
    |> Seq.map (fun (row, col, id) -> id)
    |> Seq.max

