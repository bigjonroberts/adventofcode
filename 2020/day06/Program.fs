
let rec readGroup (sr: System.IO.StreamReader) (group: string list) =
    if sr.EndOfStream then
        group
    else
        let line = sr.ReadLine ()
        if line = "" then
            group
        else
            readGroup sr (line::group)

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield readGroup sr List.empty }

let countChar (charCount: Set<char>) (c: char) =
    Set.add c charCount

module Part2 =
    let countChar (charCount: Set<char>) (answers: char seq) =
        answers
        |> Seq.filter (fun c -> Set.contains c charCount)
        |> Set.ofSeq

[<EntryPoint>]
let main argv =
    let groups = "input.txt" |> readLines |> Seq.cache
    groups
    |> Seq.map (
        List.toSeq >> (Seq.collect id) // converts to a single line
        >> Seq.fold countChar Set.empty<char>) // counts
    |> Seq.sumBy Set.count
    |> printfn "Part 1: %i"

    groups
    |> Seq.map (fun group ->
        let firstSet = group |> Seq.head |> Set.ofSeq
        group
        |> Seq.tail
        |> Seq.fold Part2.countChar firstSet)
    |> Seq.sumBy Set.count
    |> printfn "Part 2: %i"

    0