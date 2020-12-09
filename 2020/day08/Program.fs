
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
        if instructionNumber = (Array.length bootCode) then
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

    let flipNext startIndex bootCode =
        let mutable flipped = false
        bootCode
        |> Array.map (fun operation ->
            if not flipped then
                match operation with
                | NoOp (i, x) ->
                    if i > startIndex then
                        flipped <- true
                        Jump (i, x)
                    else operation
                | Jump (i, x) ->
                    if i > startIndex then
                        flipped <- true
                        NoOp (i, x)
                    else operation
                | Accumulate _ -> operation
            else
                operation
        )

    let rec boot accumulator (usedCodes: Set<int>) (instructionNumber: int) bootCode =
        match Array.tryItem instructionNumber bootCode with
        | None ->
            if instructionNumber = (Array.length bootCode) then
                Some accumulator
            else
                failwithf "Instruction number %i does not exist" instructionNumber
        | Some (Accumulate (i,x)) ->
            if usedCodes.Contains i then
                None
            else
                boot (accumulator + x) (Set.add i usedCodes) (instructionNumber + 1) bootCode
        | Some (Jump (i,x)) ->
            if usedCodes.Contains i then
                None
            else
                boot accumulator (Set.add i usedCodes) (instructionNumber + x) bootCode
        | Some (NoOp (i, _)) ->
            if usedCodes.Contains i then
                None
            else
                boot accumulator (Set.add i usedCodes) (instructionNumber + 1) bootCode


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

    printfn "Instruction count: %i" (Array.length bootCode)
    seq { 0 .. (Array.length bootCode - 1) }
    |> Seq.choose (fun i ->
        Part2.flipNext i bootCode
        |> Part2.boot 0 Set.empty 0 )
    |> Seq.head
    |> printfn "Part 2: %i"

    0
