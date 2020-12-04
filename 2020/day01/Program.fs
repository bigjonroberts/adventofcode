open System

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let findTotal (targetTotal:int) xs =
    xs
    |> Array.indexed
    |> Array.choose (fun (index,value) -> 
        xs
        |> Array.skip index
        |> Array.indexed
        |> Array.choose (fun (index1,value1) ->
            xs
            |> Array.skip index1
            |> Array.indexed
            |> Array.choose (fun (index2,value2) -> if value + value1 + value2  = targetTotal then Some (value, value1, value2) else None)
            |> Array.tryHead)
        |> Array.tryHead)
    |> Array.tryHead

[<EntryPoint>]
let main argv =

    let input = "input.txt" |> readLines |> Seq.map int |> Array.ofSeq
    input
    |> findTotal 2020
    |> Option.map (fun (x,y,z) -> x * y * z)
    |> Option.iter (printfn "Answer: %i")

    0