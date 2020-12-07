
let rec readPassport (sr: System.IO.StreamReader) (passport: string list) =
    if sr.EndOfStream then
        passport
    else
        let line = sr.ReadLine ()
        if line = "" then
            passport
        else
            readPassport sr (line::passport)

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield readPassport sr List.empty }

let parse (passPort: string list) =
    System.String.Join(' ', Array.ofList passPort)
    |> fun s -> s.Split(' ')
    |> Array.map (fun s -> s.Split(':') |> Array.head)
    |> Array.sort
    |> Array.filter ((<>) "cid")
    |> List.ofArray

let reqCodes =
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    |> List.sort

let validate (reqCodes: string list) (parsedPassportKeys: string list) =
    (reqCodes.Length = parsedPassportKeys.Length)
    &&  (List.zip reqCodes parsedPassportKeys
        |> List.forall (fun (x,y) -> x = y))

[<EntryPoint>]
let main argv =
    readLines "input"
    |> Seq.map parse
    |> Seq.filter (validate reqCodes)
    |> Seq.length
    |> printfn "Part 1: %i"
    0
