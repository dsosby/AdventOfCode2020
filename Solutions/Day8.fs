module Day8

open System.Collections
open FParsec
let parse = run // Rename as this is kind of confusing in this context

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
      hitmap: BitArray
      pc: int
      acc: int64
      }

let loadProgram (input: string) =
    let parsed = parse program input
    match parse program input with
    | Success (ast, _, _) ->
        let program = ast |> List.toArray
        Some { program = program
               hitmap = BitArray(program.Length)
               pc = 0
               acc = int64 0 }
    | Failure (errMsg, _, _) ->
        eprintf "%s" errMsg; None

let execute computer =
    let rec runNext computer =
        // TODO Better infinite loop detection?
        if computer.pc >= computer.program.Length then computer.acc
        else if computer.hitmap.[computer.pc] then computer.acc
        else
            let computer = { computer with hitmap = immutableSetBit computer.hitmap computer.pc true }

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

// Part 2

// Copying from before, but updating to type the result and support DFS for Success
// Also, refactored the operations out

type ExecutionResult =
| Success of acc: int64
| InfiniteLoop
| Failure

let incPc computer = { computer with pc = computer.pc + 1 }
let incAcc computer inc = { computer with acc = computer.acc + inc }
let jmpPc computer offset = { computer with pc = computer.pc + offset }

// Is there a function for this? This seems common/re-usable
let isSuccess = function
    | Success x -> true
    | _ -> false

let reduceState computer op =
    match op with
    | NoOp _ ->
        computer |> incPc
    | Jump offset ->
        computer |> jmpPc <| offset
    | Accumulate increment ->
        computer |> incPc |> incAcc <| int64 increment

let executeWithSuccessFind computer =
    let rec runNext computer =
        // TODO Better infinite loop detection?
        if computer.pc >= computer.program.Length then Success(computer.acc)
        else if computer.hitmap.[computer.pc] then InfiniteLoop
        else
            let computer = { computer with hitmap = immutableSetBit computer.hitmap computer.pc true }
            printfn "%d" computer.pc

            // Given a nop, push [nop, jmp]
            // Given a jmp, push [jmp, nop]
            // DFS for SuccessResult

            // TODO Issue -- the puzzle is change _exactly one_ instruction
            // This implementation changes _many_
            // Store if we've already made a "swap" (mapi and idx > 0?)
            // If so, no more swaps allowed

            let possibleOps =
                match computer.program.[computer.pc] with
                | NoOp x -> [NoOp(x); Jump(x)]
                | Jump x -> [Jump(x); NoOp(x)]
                | x -> [x]

            let executeOp op = reduceState computer op |> runNext

            let results = possibleOps |> List.map executeOp
            let successResult = results |> List.tryFind isSuccess
            Option.defaultValue InfiniteLoop successResult
        
    runNext computer

[<Solution(part = 2)>]
let findSuccessfulExecution (input: string) =
    // Switch _exactly one_ nop->jmp OR a jmp->nop
    // One of these will produce a Success
    loadProgram input
    |> Option.map executeWithSuccessFind
