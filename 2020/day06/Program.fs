
let rec readGroup (sr: System.IO.StreamReader) (group: string) =
    if sr.EndOfStream then
        group
    else
        let line = sr.ReadLine ()
        if line = "" then
            group
        else
            readGroup sr (group + line)

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield readGroup sr "" }

let countChar (charCount: Set<char>) (c: char) =
    Set.add c charCount

[<EntryPoint>]
let main argv =
    readLines "input.txt"
    |> Seq.map ( Seq.fold countChar Set.empty<char>)
    |> Seq.sumBy (Set.count)
    |> printfn "Part 1: %i"
    0
