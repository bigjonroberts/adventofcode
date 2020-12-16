
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let tribinacci () =
    let cache = new System.Collections.Generic.Dictionary<int, int64 seq>()
    let rec trib i x last3 =
        match cache.TryGetValue x with
        | true, v -> v
        | false, _ ->
            let next3 = seq { yield! Seq.tail last3; yield Seq.sum last3 }
            cache.Add (x,next3)
            if i = x then
                next3
            else
                trib (i+1) x next3
    function
    | 0 -> 0L
    | 1 -> 1L
    | 2 -> 2L
    | 3 -> 4L
    | x ->
        match cache.TryGetValue (x-1) with
        | true, last3 ->
            trib (x-1) x last3
            |> Seq.last
        | false, _ ->
            trib 4 x [ 1L; 2L; 4L; ]
            |> Seq.last

[<EntryPoint>]
let main argv =

    let input =
        "input"
        |> readLines
        |> Seq.map int
        |> Seq.cache
        |> Seq.append [ 0 ]

    input
    |> Seq.sortDescending
    |> Seq.pairwise
    |> Seq.map ((<||) (-))
    |> Seq.append [ 3 ]
    |> Seq.sort
    |> Seq.countBy id
    |> Seq.map snd
    |> Seq.reduce (*)
    |> printfn "Part 1: %i"

    let tribinacci = tribinacci () // initializes cache

    input
    |> Seq.sort
    |> Seq.pairwise
    |> Seq.map (((<||) (-)) >> ((*) (-1))) 
    |> fun xs -> seq { yield! xs; yield 3 }
    |> Seq.fold (fun (accum: int64, oneJoltDifferences) x -> 
        match (x, oneJoltDifferences) with
        | (1,j) -> (accum, j + 1)
        | (_,0) -> (accum, 0)
        | (x,j) -> 
            (accum * (tribinacci j), 0)
        ) (1L,0)
    |> fst
    |> printfn "Part 2: %i"


    0
