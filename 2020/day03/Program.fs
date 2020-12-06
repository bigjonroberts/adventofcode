
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let slide (position: int, treeCount: int) (treeMap: char seq)  =
    let width = Seq.length treeMap
    let position = if width > 0 then position % width else position
    let treeCount =
        if Seq.item position treeMap = '#' then
            treeCount + 1
        else
            treeCount
    (position + 3, treeCount)

[<EntryPoint>]
let main argv =
    "input.txt"
    |> readLines
    |> Seq.fold slide (0,0)
    |> snd
    |> printfn "Part 1: %i"
    0
