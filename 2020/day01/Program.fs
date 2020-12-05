open System

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let findTotal (targetTotal:int) xs =
    let xs2 = xs
    xs
    |> Array.indexed
    |> Array.choose (fun (index,value) -> 
        xs2
        |> Array.skip index
        |> Array.choose (fun x -> if x + value = targetTotal then Some x else None)
        |> Array.tryHead
        |> Option.map (fun x -> x,value))
    |> Array.tryHead

[<EntryPoint>]
let main argv =

    let input = "input.txt" |> readLines |> Seq.map int |> Array.ofSeq
    input
    |> findTotal 2020
    |> Option.map (fun (x,y) -> x * y)
    |> Option.iter (printfn "Answer: %i")

    0