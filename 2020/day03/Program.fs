
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let slide (xslide: int, yslide: int) (xposition: int, yposition: int, treeCount: int64) (treeMap: char seq)  =
    if yposition % yslide = 0 then
        let width = Seq.length treeMap
        let position = if width > 0 then xposition % width else xposition
        let treeCount =
            if Seq.item position treeMap = '#' then
                treeCount + 1L
            else
                treeCount
        (xposition + xslide, yposition + 1, treeCount)
    else
        (xposition, yposition + 1, treeCount)

let calcTrees input slope =
    input
    |> Seq.fold (slide slope) (0,0,0L)
    |> fun (_,_,x) -> x

[<EntryPoint>]
let main argv =
    let input = "input.txt" |> readLines |> Seq.cache
    let calcTrees = calcTrees input

    calcTrees (3,1)
    |> printfn "Part 1: %i"

    [ (1,1); (3,1); (5,1); (7,1); (1,2) ]
    |> List.map (calcTrees >> fun x -> printfn "%i" x; x)
    |> List.reduce (*)
    |> printfn "Part 2: %i"

    0