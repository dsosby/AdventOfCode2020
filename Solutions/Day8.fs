module Day8

open FParsec
let parse = run // Rename as this is kind of confusing

// Domain

type Operation =
| NoOp of int
| Accumulate of increment: int
| Jump of offset: int

// Parsers

let singleIntOp opcode = pstring opcode .>> spaces1 >>. pint32

let statement =
    choice [
        singleIntOp "nop" |>> NoOp
        singleIntOp "acc" |>> Accumulate
        singleIntOp "jmp" |>> Jump
    ]
let program = sepEndBy statement newline .>> eof

// Computer

type ComputerState =
    { program: Operation array
      hitmap: bool array // Probably inefficient
      pc: int
      acc: int64 }

let loadProgram (input: string) =
    let parsed = parse program input
    match parse program input with
    | Success (ast, _, _) ->
        let program = ast |> List.toArray
        Some { program = program
               hitmap = Array.create program.Length false
               pc = 0
               acc = int64 0 }
    | Failure (errMsg, _, _) ->
        eprintf "%s" errMsg; None

let execute computer =
    let rec runNext computer =
        // TODO Better infinite loop detection?
        if computer.hitmap.[computer.pc] then computer.acc
        else if computer.pc >= computer.program.Length then computer.acc
        else
            computer.hitmap.[computer.pc] <- true

            match computer.program.[computer.pc] with
            | NoOp _ -> runNext { computer with pc = computer.pc + 1 }
            | Accumulate increment ->
                runNext { computer with
                            pc = computer.pc + 1
                            acc = computer.acc + (int64 increment) }
            | Jump offset -> runNext { computer with pc = computer.pc + offset }
        
    runNext computer

[<Solution(part = 1)>]
let executeToLoop (input: string) =
    loadProgram input
    |> Option.map execute

