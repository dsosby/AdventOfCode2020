module Day3

open Utilities

let sample = @"..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#"

type Location =
| Tree
| Snow

let coordIter slope =
    Seq.initInfinite (fun idx ->
        (fst slope) * idx, (snd slope) * idx
    )

let solvePart1 (input : string) =
    let puzzleWidth = input.IndexOf '\n' + 1
    let puzzleHeight = input.Length / puzzleWidth

    let locations =
        coordIter (3, 1)
        |> Seq.takeWhile (fun (x, y) -> y <= puzzleHeight)
        |> Seq.map (fun (x, y) -> 
            let mappedX = x % (puzzleWidth - 1) // -1 to exclude \n
            let mappedY = y * puzzleWidth
            let idx = mappedY + mappedX
            printfn "Checking %d, %d (idx=%d) (max=%d) [%c]" x y idx input.Length input.[idx]
            input.[idx])

    // TODO Need to debug this, worked with my sample input but not big input... I think maybe Trims as the \n was getting in the way or something?

    locations
    |> countIf (charIs '#')
    |> Some

let inputToMatrix (input : string) =
    input.Trim().Split [| '\n' |]
    |> Seq.map (fun x -> x.Trim())
    |> Seq.map Seq.toArray
    |> Seq.toArray

let calculateTreesHit (input : string) slope =
    // Index into a matrix since I suck at off-by-one
    let map = inputToMatrix input
    let rows = map.Length
    let cols = map.[0].Length

    let locations = 
        coordIter slope
        |> Seq.takeWhile (fun (x, y) -> y < rows)
        |> Seq.map (fun (x, y) ->
            let mapX = x % cols
            let mapY = y

            let location = map.[mapY].[mapX]
            let locationDebug =
                match location with
                | '#' -> "X"
                | _ -> "O"

            let debugString =
                System.String(map.[mapY].[0..mapX-1]) +
                locationDebug +
                System.String(map.[mapY].[mapX+1..])
            let replicationColumn = x / cols
            // printfn "(%d, %d)-%d\t %s" mapX mapY replicationColumn debugString

            location
           )

    locations
    |> countIf (charIs '#')

[<Solution(part = 1)>]
let solvePart1Again (input : string) =
    Some (calculateTreesHit input (3, 1))

[<Solution(part = 2)>]
let solvePart2 (input : string) =
    let slopes = [
        1,1;
        3,1;
        5,1;
        7,1;
        1,2;
    ]
    let calculation = calculateTreesHit input

    slopes
    |> Seq.map calculation
    |> Seq.fold (fun acc (nxt : int) -> acc * (bigint nxt)) (bigint 1)
    |> Some
