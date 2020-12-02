module AdventOfCode2020.Solutions

let solutions = dict[
    "day1part1", Day1.solvePart1;
    "day1part2", Day1.solvePart2;
    "day2part1", Day2.solvePart1;
    "day2part2", Day2.solvePart2;
]

let solve key input =
    match input with
    | (null|"") -> "This should be an HTTP 400"
    | input ->
        let solver = solutions.[key]
        match (solver input) with
        | None -> "This should be an HTTP 404"
        | Some solution -> solution.ToString()
