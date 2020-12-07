
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
    |> Array.map (fun s ->
        let a = s.Split(':')
        (a.[0], a.[1]))
    |> Array.sort
    |> Array.filter (fst >> ((<>) "cid"))
    |> List.ofArray

let reqCodes =
    [ "byr"; "iyr"; "eyr"; "hgt"; "hcl"; "ecl"; "pid" ]
    |> List.sort

let validate (reqCodes: string list) (parsedPassportKeys: (string * string) list) =
    (reqCodes.Length = parsedPassportKeys.Length)
    &&  (List.zip reqCodes (List.map fst parsedPassportKeys)
        |> List.forall (fun (x,y) -> x = y))

let checkYear (minYear: int) (maxYear: int) (yearString: string) =
    match System.Int32.TryParse yearString with
    | (true,year) when minYear <= year && year <= maxYear -> true
    | _ -> false 

type HeightParsing = {
    Digits: char list
    Measure: HeightMeasure
}
and HeightMeasure = | Cm | In | Unknown

let validValue (key: string, value: string) =
    match key with
    | "byr" -> // (Birth Year) - four digits; at least 1920 and at most 2002.
        checkYear 1920 2002 value
    | "iyr" -> // (Issue Year) - four digits; at least 2010 and at most 2020.
        checkYear 2010 2020 value
    | "eyr" -> // (Expiration Year) - four digits; at least 2020 and at most 2030.
        checkYear 2020 2030 value
    | "hgt" -> // (Height) - a number followed by either cm or in:
                // If cm, the number must be at least 150 and at most 193.
                // If in, the number must be at least 59 and at most 76.
        value
        |> Seq.fold(fun heightParsing c ->
            let mp f = Option.map f heightParsing
            match (c, heightParsing |> Option.map (fun hp -> hp.Measure)) with
            | (c, Some Unknown) when '0' <= c && c <= '9' -> mp (fun hp -> { hp with Digits = c::hp.Digits } )
            | ('c', _) | ('m', _) -> mp (fun hp -> { hp with Measure = Cm } )
            | ('i', _) | ('n', _) -> mp (fun hp -> { hp with Measure = In } )
            | _ -> None (* invalid *) ) (Some { Digits = List.empty; Measure = Unknown })
        |> (Option.bind (fun hp ->
                match hp.Digits |> List.rev |> System.String.Concat |> System.Int32.TryParse with
                | (true, height) ->
                    match (hp.Measure, height) with
                    | (Cm, hgt) when 150 <= hgt && hgt <= 193 -> Some hp
                    | (In, hgt) when 59 <= hgt && hgt <= 76 -> Some hp
                    | _ -> None
                | _ -> None))
        |> function | Some _ -> true | None -> false
    | "hcl" -> // (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
        match value |> List.ofSeq with
        | '#'::tail when List.length tail = 6 && tail |> List.forall (fun c -> ('0' <= c && c <= '9') || ('a' <= c && c <= 'f') ) -> true
        | _ -> false
    | "ecl" -> // (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
        List.contains value [ "amb"; "blu"; "brn"; "gry"; "grn"; "hzl"; "oth" ]
    | "pid" -> // (Passport ID) - a nine-digit number, including leading zeroes.
        match (String.length value = 9, System.Int64.TryParse value) with
        | (true,(true, _)) -> true
        | _ -> false
    | _ -> false

[<EntryPoint>]
let main argv =
    let lines = "input" |> readLines |> Seq.cache

    lines
    |> Seq.map parse
    |> Seq.filter (validate reqCodes)
    |> Seq.length
    |> printfn "Part 1: %i"

    lines
    |> Seq.map parse
    |> Seq.filter (validate reqCodes)
    |> Seq.filter (List.forall validValue)
    |> Seq.length
    |> printfn "Part 1: %i"
    0
