
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let memoize fn =
  let cache = new System.Collections.Generic.Dictionary<_,_>()
  (fun x ->
    match cache.TryGetValue x with
    | true, v -> v
    | false, _ -> let v = fn (x)
                  cache.Add(x,v)
                  v)

let tribinacci =
    function
    | 0 -> 0L
    | 1 -> 1L
    | 2 -> 2L
    | 3 -> 4L
    | x ->
        let rec trib i last3 =
            let nextVal = Seq.sum last3
            if i = x then
                nextVal
            else
                seq { yield! Seq.tail last3; yield nextVal }
                |> trib (i+1)
        [ 1L; 2L; 4L; ] |> trib 4
    |> memoize

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
