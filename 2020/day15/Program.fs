
let parse (s: string) =
    s.Split(',')
    |> Array.map int
    |> Array.rev
    |> List.ofArray 

// let rec gen i input = seq {
//     // printfn "%O" input
//     if (i % 1000000 = 0) then printfn "%i" i
//     let lastNum :: remainder = input
//     let nextNum =
//         remainder
//         |> List.tryFindIndex ((=) lastNum)
//         |> Option.map ((+) 1)
//         |> Option.defaultValue 0
//     yield nextNum
//     yield! gen (i+1) (nextNum :: input)
// }

let rec gen i lastNum input = seq {
    yield lastNum
    if (i % 1000000 = 0) then printfn "%i" i
    let nextNum =
        input
        |> Map.tryFind lastNum
        |> Option.map (fun x -> i - x)
        |> Option.defaultValue 0
    yield! gen (i+1) nextNum (Map.add lastNum i input)
}

let genSequence input =
    let lastNum :: remainder = input
    let dict =
        remainder
        |> List.rev
        |> List.indexed
        |> List.map (fun (x,y) -> (y,x))
        |> Map.ofList
    seq {
        yield! List.rev remainder
        yield! gen (List.length remainder) lastNum dict
    }

let findVal target input =
    input
    |> parse
    |> genSequence
    |> Seq.take target
    |> Seq.last

[<EntryPoint>]
let main argv =

    findVal 2020 "0,3,6"
    |> printfn "Part 1 Test Output: %i"

    // Given the starting numbers 1,3,2, the 2020th number spoken is 1.
    findVal 2020 "1,3,2"
    |> printfn "Part 1 Test Output: %i"
    // Given the starting numbers 2,1,3, the 2020th number spoken is 10.
    findVal 2020 "2,1,3"
    |> printfn "Part 1 Test Output: %i"
    // Given the starting numbers 1,2,3, the 2020th number spoken is 27.
    findVal 2020 "1,2,3"
    |> printfn "Part 1 Test Output: %i"
    // Given the starting numbers 2,3,1, the 2020th number spoken is 78.
    findVal 2020 "2,3,1"
    |> printfn "Part 1 Test Output: %i"
    // Given the starting numbers 3,2,1, the 2020th number spoken is 438.
    findVal 2020 "3,2,1"
    |> printfn "Part 1 Test Output: %i"
    // Given the starting numbers 3,1,2, the 2020th number spoken is 1836.
    findVal 2020 "3,1,2"
    |> printfn "Part 1 Test Output: %i"
    
    findVal 2020 "1,0,18,10,19,6"
    |> printfn "Part 1: %i"

    findVal 30000000 "1,0,18,10,19,6"
    |> printfn "Part 2: %i"

    0