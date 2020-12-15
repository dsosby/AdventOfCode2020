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
      acc: int64 }

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

let isSuccess result = match result with Success _ -> true | _ -> false

let incPc computer = { computer with pc = computer.pc + 1 }
let incAcc inc computer = { computer with acc = computer.acc + (int64 inc) }
let jmpPc offset computer = { computer with pc = computer.pc + offset }

let reduceState computer op =
    match op with
    | NoOp _ ->
        computer |> incPc
    | Jump offset ->
        computer |> jmpPc offset
    | Accumulate increment ->
        computer |> incPc |> incAcc increment

let executeWithCuring computer =
    let rec runNext allowCuring computer =
        // TODO Better infinite loop detection?
        if computer.pc >= computer.program.Length then Success(computer.acc)
        else if computer.hitmap.[computer.pc] then InfiniteLoop
        else
            let computer = { computer with hitmap = immutableSetBit computer.hitmap computer.pc true }

            let executeOp allowCuring op = reduceState computer op |> runNext allowCuring
            let op = computer.program.[computer.pc]
            let result = executeOp allowCuring op

            // TODO Instead of mutating an existing function, how could I have composed?
            match allowCuring, op, result with
            | true, NoOp v, InfiniteLoop -> executeOp false (Jump(v))
            | true, Jump v, InfiniteLoop -> executeOp false (NoOp(v))
            | _, _, r -> r

    runNext true computer

[<Solution(part = 2)>]
let findSuccessfulExecution (input: string) =
    // Switch _exactly one_ nop->jmp OR a jmp->nop
    // One of these will produce a Success
    loadProgram input
    |> Option.map executeWithCuring
