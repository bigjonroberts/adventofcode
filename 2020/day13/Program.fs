
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let testinput = seq { "939"; "7,13,x,x,59,x,31,19" }

let parseInput (s: string seq) =
    match List.ofSeq s with
    | a :: buses ->
        let a = int64 a
        let buses = (System.String.Concat(List.head buses)).Split(',')
        (a, buses)
    | _ -> failwith "at least two lines expected for input"

let parseBuses = Array.filter ((<>) "x") >> Array.map int64

let rec genTimeStamps step (nextVal: int64) = seq {
    yield nextVal
    yield! genTimeStamps step (nextVal + step)
}

let rec findFirstAfter arrivalTime busNo = // busTime =
    genTimeStamps 1L arrivalTime
    |> Seq.find (fun x -> x % busNo = 0L)

    // if busTime > arrivalTime then
    //     busTime
    // else
    //     findFirstAfter arrivalTime busNo (busTime + busNo)

module Part2 =

    type Bus = {
        Position: int64
        BusNo: int64
    }

    let testinput =
        [   "7,13,x,x,59,x,31,19"
            "17,x,13,19"
            "67,7,59,61"
            "67,7,x,59,61"
            "1789,37,47,1889" ]

    let parseBuses (xs: string []) =
        xs
        |> Array.indexed
        |> Array.filter (snd >> ((<>) "x"))
        |> Array.map (fun (i,b) -> { Position = int64 i; BusNo = int64 b })

    let findTimeStamp (buses: Bus list) =
        let rec find (buses: Bus list) (step: int64) (minTime: int64) =
            match buses with
            | bus :: theRest ->
                // printfn "%O; step: %i; minTime: %i" bus step minTime
                genTimeStamps step minTime
                |> Seq.find (fun timeStamp -> ((timeStamp + bus.Position) % bus.BusNo = 0L))
                |> find theRest (step * bus.BusNo)
            | [] -> minTime       

        find buses 1L 1L


[<EntryPoint>]
let main argv =

    let (arrivalTime, buses) =
        "input"
        |> readLines
        |> parseInput

    buses
    |> parseBuses
    |> Array.map (fun busNo -> (busNo, findFirstAfter arrivalTime busNo))
    |> Array.sortBy snd
    |> Array.head
    |> fun (busNo, departTime) -> printfn "Part 1: %i" (busNo * (departTime - arrivalTime))

    Part2.testinput
    |> Seq.map (fun s -> s.Split(',') |> Part2.parseBuses |> List.ofArray |> Part2.findTimeStamp)
    |> Seq.iter (printfn "Part 2 test data solution: %i")

    buses
    |> Part2.parseBuses
    |> List.ofArray
    |> Part2.findTimeStamp
    |> printfn "Part 2: %i"

    0