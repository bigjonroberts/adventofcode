
type ParseMode =
    | FieldDef
    | YourTicket
    | NearbyTickets

type FieldDef = {
    Name: string
    Range1: int * int
    Range2: int * int
}

module FieldDef =
    let validValue x fieldDef =
        let (r1min, r1max) = fieldDef.Range1
        let (r2min, r2max) = fieldDef.Range2
        (r1min <= x && x <= r1max) || (r2min <= x && x <= r2max)

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let classify parseMode line =
    match (parseMode, line) with
    | (FieldDef, "") -> ((YourTicket, None), YourTicket)
    | (FieldDef, line) -> ((FieldDef, Some line), FieldDef)
    | (YourTicket, "") -> ((NearbyTickets, None), NearbyTickets)
    | (YourTicket, "your ticket:") -> ((YourTicket, None), YourTicket)
    | (YourTicket, line) -> ((YourTicket, Some line), YourTicket)
    | (NearbyTickets, "nearby tickets:") -> ((NearbyTickets, None), NearbyTickets)
    | (NearbyTickets, line) -> ((NearbyTickets, Some line), NearbyTickets)

let parse (fieldDefs, yourTicket, nearbyTickets) (parseMode, line: string) =
    match parseMode with
    | FieldDef ->
        let segments = (line.Split(": "))
        let ranges =
            segments.[1].Split(" or ")
            |> Array.map (fun s ->
                let r = s.Split("-")
                (int r.[0], int r.[1]) )
        let fieldDef =
            { Name = segments.[0]; Range1 = ranges.[0]; Range2 = ranges.[1] }
        (fieldDef :: fieldDefs, yourTicket, nearbyTickets)
    | YourTicket -> (fieldDefs, line.Split(',') |> Array.map int, nearbyTickets)
    | NearbyTickets -> (fieldDefs, yourTicket, (line.Split(',') |> Array.map int) :: nearbyTickets)

let invalidValues (fieldDefs: List<FieldDef>) =
    Array.filter (fun (x: int) ->
        fieldDefs |> List.exists (FieldDef.validValue x) |> not)

let testInput = [
    "class: 1-3 or 5-7"
    "row: 6-11 or 33-44"
    "seat: 13-40 or 45-50"
    ""
    "your ticket:"
    "7,1,14"
    ""
    "nearby tickets:"
    "7,3,47"
    "40,4,50"
    "55,2,20"
    "38,6,12" ]

module Part2 =
    let isValid (fieldDefs: List<FieldDef>) =
        Array.forall (fun (x: int) ->
            fieldDefs |> List.exists (FieldDef.validValue x))

    let testInput = [
        "class: 0-1 or 4-19"
        "row: 0-5 or 8-19"
        "seat: 0-13 or 16-19"
        ""
        "your ticket:"
        "11,12,13"
        ""
        "nearby tickets:"
        "3,9,18"
        "15,1,5"
        "5,14,9" ]

[<EntryPoint>]
let main _ =

    let (fieldDefs, yourTicket, nearbyTickets) =
        // Part2.testInput
        "input"
        |> readLines
        |> Seq.mapFold classify FieldDef
        |> fst
        |> Seq.choose (fun (p,s) -> s |> Option.map (fun s -> (p,s)))
        |> Seq.fold parse (List.empty, Array.empty, List.empty)
    
    nearbyTickets
    |> Array.ofList
    |> Array.collect (invalidValues fieldDefs)
    |> Array.sum
    |> printfn "Part 1: %i"

    let validTickets = List.filter (Part2.isValid fieldDefs) nearbyTickets

    fieldDefs
    |> List.sortBy (fun fd -> // count the number of valid field positions for each field definition and sort by it
        seq { 0 .. (List.length fieldDefs - 1) }
        |> Seq.filter (fun x ->
            (validTickets |> List.forall (fun t -> FieldDef.validValue t.[x] fd)))
        |> Seq.length)
    |> List.fold (fun fieldMap fd -> // now allocate a field definition to each field position
        seq { 0 .. (List.length fieldDefs - 1) }
        |> Seq.find (fun x ->
            (not (Map.containsKey x fieldMap)) &&
            (validTickets |> List.forall (fun t -> FieldDef.validValue t.[x] fd)))
        |> fun x -> Map.add x fd fieldMap
        ) Map.empty
    |> Map.filter (fun _ fd -> fd.Name.StartsWith("departure"))
    |> Map.toSeq
    |> Seq.map (fun (key,_) -> uint64 yourTicket.[key])
    |> Seq.reduce (*)
    |> printfn "Part 2: %i"

    0
