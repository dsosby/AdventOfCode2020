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

let parseSeats (input: string) =
    input.Trim().Split("\n")
    |> Seq.map findSeat

[<Solution(part = 1)>]
let findMaxSeatId (input: string) =
    input
    |> parseSeats
    |> Seq.map (fun (row, col, id) -> id)
    |> Seq.max

[<Solution(part = 2)>]
let findMissingValue (input: string) =
    let seats =
        input
        |> parseSeats
        |> Seq.sort
        |> Seq.toList

    // Find the seat where id - offset != idx
    // No Seq.findi, so temp do it with mapi and find
    let (_, _, offset) = seats.[0]
    let (_, outOfOrderSeat) =
        seats
        |> Seq.mapi (fun idx (_, _, seatId) -> idx, seatId)
        |> Seq.find (fun (idx, seatId) -> (seatId - offset) <> idx)

    outOfOrderSeat - 1
