open System

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let findTotal (targetTotal:int) (numFactors: int) xs =
    let xs = Array.indexed xs
    let rec recFindTotal (index: int) (depth: int) (factors: int list) =
        let calculate (index,value) =
            if depth = numFactors then
                let newFactors = value::factors
                if newFactors |> List.reduce (+)  = targetTotal then
                    Some newFactors
                else None
            else
                recFindTotal index (depth+1) (value::factors)
        xs
        |> Array.skip index
        |> Array.choose calculate
        |> Array.tryHead
    recFindTotal 0 0 List.empty

let calcProduct input i =
    input
    |> findTotal 2020 i
    |> Option.map (List.reduce (*))
    |> Option.iter (printfn "Answer part %i: %i" i)

[<EntryPoint>]
let main argv =

    let input = "input.txt" |> readLines |> Seq.map int |> Array.ofSeq
    { 1 .. 2 } |> Seq.iter (calcProduct input)
    0