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

    locations
    |> countIf (charIs '#')
    |> Some

let inputToMatrix (input : string) =
    input.Trim().Split [| '\n' |]
    |> Seq.map Seq.toArray
    |> Seq.toArray

let solvePart1Again (input : string) =
    // Index into a matrix since I suck at off-by-one
    let map = inputToMatrix input
    let rows = map.Length
    let cols = map.[0].Length - 1
    let slope = (3, 1)

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
            printfn "(%d, %d)-%d\t %s" mapX mapY replicationColumn debugString

            location
           )

    locations
    |> countIf (charIs '#')
    |> Some

