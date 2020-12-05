module AdventOfCode2020.Solutions

// TODO Move the Runner reflection solution to this assembly so it is accessible by Web

// TODO Find a better way to deal with this... each solution having their own type.
// Can I not have a Map of IFormattable option or something?
// TODO Instead of registering, can I use reflection and attributes?
let solutions = dict[
    "day1part1", Day1.solvePart1 >> Option.map (sprintf "%A");
    "day1part2", Day1.solvePart2 >> Option.map (sprintf "%A");
    "day2part1", Day2.solvePart1 >> Option.map (sprintf "%A");
    "day2part2", Day2.solvePart2 >> Option.map (sprintf "%A");
    "day3part1", Day3.solvePart1Again >> Option.map (sprintf "%A");
    "day3part2", Day3.solvePart2 >> Option.map (sprintf "%A");
]

let solve key input =
    match input with
    | (null|"") -> "This should be an HTTP 400"
    | input ->
        let solver = solutions.[key]
        match (solver input) with
        | None -> "This should be an HTTP 404"
        | Some solution -> solution.ToString()
