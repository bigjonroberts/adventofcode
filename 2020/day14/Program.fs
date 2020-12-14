
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

type Mask = string

type Register = {
    Mask: char seq
    Address: int64
    Value: uint64
}

let apply (prog: Map<int64,char seq>) (instruction: Register) =
    seq {
        yield! (int64 instruction.Value, 2) |> System.Convert.ToString |> Seq.rev
        for i = 0 to 35 do yield '0' }
    |> Seq.take 36
    |> Seq.rev
    |> Seq.zip instruction.Mask
    |> Seq.map (fun (m,c) -> match m with | 'X' -> c | x -> x) 
    |> fun v ->
        Map.add instruction.Address v prog

let parse (mask: Mask option) (line: string) =
    let segments = line.Split(" = ")
    match (mask, segments.[0]) with
    | (_, "mask") ->
        (None, Some segments.[1] )
    | (Some mask', s) ->
        let address =
            s
            |> Seq.filter (fun c -> '0' <= c && c <= '9')
            |> System.String.Concat
            |> int64
        (Some { Mask = mask'; Address = address; Value = uint64 segments.[1] }, Some mask')
    | _ -> failwith "Invalid input"

let testInput = seq {
    "mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X"
    "mem[8] = 11"
    "mem[7] = 101"
    "mem[8] = 0" }


let processInput apply mapResult =
    Seq.mapFold parse None
    >> fst
    >> Seq.choose id
    >> Seq.fold apply Map.empty
    >> Map.toSeq
    >> Seq.map mapResult
    >> Seq.reduce (+)

module Part2 =
    let rec private write (address: char seq) (floating: List<int>) (value: uint64) (prog: Map<uint64,uint64>) =
        match floating with
        | x :: tail ->
            let setBit value = address |> Seq.mapi (fun i c -> if i = x then value else c)
            prog
            |> write (setBit '0') tail value
            |> write (setBit '1') tail value
        | [] -> Map.add (System.Convert.ToUInt64(System.String.Concat address, 2)) value prog

    let rec private calcAddress (floating: Option<int list>) (address: char seq) = seq {
        match floating with
        | None ->
            let floating =
                address
                |> Seq.indexed
                |> Seq.filter (snd >> (fun c -> c = 'X'))
                |> Seq.map fst
                |> List.ofSeq
            yield! calcAddress (Some floating) address
        | Some (x :: tail) ->
            let setBit value = address |> Seq.mapi (fun i c -> if i = x then value else c)
            yield! calcAddress (Some tail) (setBit '0')
            yield! calcAddress (Some tail) (setBit '1')
        | Some [] -> yield System.String.Concat address
    }

    let apply (prog: Map<uint64,uint64>) (instruction: Register) =
        seq {
            yield! (instruction.Address, 2) |> System.Convert.ToString |> Seq.rev
            for i = 0 to 35 do yield '0' }
        |> Seq.take 36
        |> Seq.rev
        |> Seq.zip instruction.Mask
        |> Seq.map (fun (m,c) -> match m with | '0' -> c | x -> x)
        |> calcAddress None
        |> Seq.fold (fun prog address -> Map.add (System.Convert.ToUInt64(address, 2)) instruction.Value prog) prog

    let testInput = seq {
        "mask = 000000000000000000000000000000X1001X"
        "mem[42] = 100"
        "mask = 00000000000000000000000000000000X0XX"
        "mem[26] = 1" }


[<EntryPoint>]
let main argv =

    testInput
    |> processInput apply (snd >> (fun (s:char seq) -> System.Convert.ToUInt64(System.String.Concat s, 2)))
    |> printfn "Part 1 Test Output: %i"

    "input"
    |> readLines
    |> processInput apply (snd >> (fun (s:char seq) -> System.Convert.ToUInt64(System.String.Concat s, 2)))
    |> printfn "Part 1: %i"
    
    Part2.testInput
    |> processInput Part2.apply snd
    |> printfn "Part 2 Test Output: %i"

    "input"
    |> readLines
    |> processInput Part2.apply snd
    |> printfn "Part 2: %i"

    0