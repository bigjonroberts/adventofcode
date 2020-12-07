
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

type Ruleset = {
    RequiredChar: char
    Min: int
    Max: int
}

type Password = {
    Password: string
    Ruleset: Ruleset
}

let parse (s: string) =
    let segments = s.Split(' ')
    let range = segments.[0].Split('-')
    {   Password = segments.[2]
        Ruleset = {
            Min = int range.[0]
            Max = int range.[1]
            RequiredChar = Seq.head segments.[1] } }

let validate (p: Password) =
    let r = p.Ruleset
    let charCount =
        p.Password
        |> Seq.sumBy (fun c -> if c = r.RequiredChar then 1 else 0)
    (r.Min <= charCount) && (charCount <= r.Max)

let validate2 (p: Password) =
    let r = p.Ruleset
    let pArray = Array.ofSeq p.Password
    [   pArray.[r.Min - 1] = r.RequiredChar
        pArray.[r.Max - 1] = r.RequiredChar ]
    |> List.sumBy (function | true -> 1 | false -> 0)
    |> (=) 1



[<EntryPoint>]
let main argv =
    let input =
        "input.txt"
        |> readLines
        |> Seq.cache
    input
    |> Seq.filter (parse >> validate)
    |> Seq.length
    |> printfn "Part 1: %i"
    input
    |> Seq.filter (parse >> validate2)
    |> Seq.length
    |> printfn "Part 2: %i"
    0
