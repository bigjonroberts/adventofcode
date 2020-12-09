
type Queue<'a> = System.Collections.Generic.Queue<'a>

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let checkSum (prior25: Queue<int64>) (x:int64) =
    let a = Array.ofSeq prior25
    seq { 0 .. 24 }
    |> Seq.tryFind (fun y ->
        let y = a.[y] 
        seq { 0 .. 24 }
        |> Seq.tryFind (fun z ->
            let z = a.[z]
            y + z = x)
        |> function Some _ -> true | None -> false)

let rec findRangeEnd (input: int64 []) (i: int) (range: int64 list) (target: int64) =
    let range = input.[i]::range
    let tot = range |> List.reduce (+)
    if tot < target then
        findRangeEnd input (i+1) range target
    else if tot = target then
        Some range
    else // tot > target
        None

[<EntryPoint>]
let main argv =
    let prior25 = Queue<int64>()
    let input =
        "input"
        |> readLines
        |> Seq.map int64
        |> Seq.cache
    let part1 =
        input
        |> Seq.indexed
        |> Seq.tryFind (fun (i,x) ->
            // printfn "index: %i; queue size: %i" i prior25.Count
            if i < 25 then
                prior25.Enqueue(x)
                false
            else
                let result =
                    checkSum prior25 x
                    |> function Some _ -> false | None -> true
                prior25.Dequeue() |> ignore
                prior25.Enqueue(x)
                result)
        |> Option.map snd
        |> Option.defaultValue 0L
    printfn "Part 1: %i" part1

    let inputArray = Array.ofSeq input
    let sumRange =
        input
        |> Seq.indexed
        |> Seq.choose (fun (i,x) ->
            printfn "choosing from (%i,%i)" i x
            findRangeEnd inputArray i List.empty part1)
        |> Seq.head
    let minVal = List.min sumRange
    let maxVal = List.max sumRange
    printfn "Part 2: %i" (minVal + maxVal)

    0 // return an integer exit code
