
type Operation =
    | Accumulate of int * int
    | Jump of int * int
    | NoOp of int * int

let parse (i: int, s: string) =
    let segments = s.Split(' ')
    match segments.[0] with
    | "acc" -> Accumulate (i, int segments.[1])
    | "jmp" -> Jump (i, int segments.[1])
    | "nop" -> NoOp (i, int segments.[1])
    | x -> failwithf "Unexpected operation code: %s" x

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let rec boot accumulator (usedCodes: Set<int>) (instructionNumber: int) bootCode =
    match Array.tryItem instructionNumber bootCode with
    | None ->
        if instructionNumber = (Array.length bootCode + 1) then
            accumulator
        else
            failwithf "Instruction number %i does not exist" instructionNumber
    | Some (Accumulate (i,x)) ->
        if usedCodes.Contains i then
            accumulator
        else
            boot (accumulator + x) (Set.add i usedCodes) (instructionNumber + 1) bootCode
    | Some (Jump (i,x)) ->
        if usedCodes.Contains i then
            accumulator
        else
            boot accumulator (Set.add i usedCodes) (instructionNumber + x) bootCode
    | Some (NoOp (i, _)) ->
        if usedCodes.Contains i then
            accumulator
        else
            boot accumulator (Set.add i usedCodes) (instructionNumber + 1) bootCode

module Part2 =

    type Flipped = Flipped of int

    let findIndex flipped =
        Array.tryFindIndex (
            function
            | NoOp _ -> false
            | Jump (i, _)
            | Accumulate (i, _) -> i > flipped
        )

    let flipNext (Flipped flipped) bootCode =
        match Array.tryItem flipped bootCode with
        | None ->
            if flipped = -1 then
                findIndex flipped
        | Some (NoOp (i, x)) ->
            Array.[i] <- Jump (i, x)
        | Some (Jump (i,_)) ->
        | Some (Accumulate _) -> bootCode

[<EntryPoint>]
let main argv =
    let bootCode =
        "input"
        |> readLines
        |> Seq.indexed
        |> Seq.map parse
        |> Array.ofSeq
    boot 0 Set.empty 0 bootCode
    |> printfn "Part 1: %i"

    0
