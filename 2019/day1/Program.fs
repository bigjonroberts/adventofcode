let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let rec totalFuel newMass baseMass =
    match newMass/3 - 2 with
    | x when x <= 0 -> baseMass
    | x -> baseMass + x |> totalFuel x
 
[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let (part1, part2, count) =
        [|  System.IO.Directory.GetCurrentDirectory ()
            "input" |]
        |> System.IO.Path.Combine
        |> readLines
        |> Seq.map (fun x ->
            let i = int x
            (i/3 - 2, totalFuel i 0))
        |> Seq.fold (fun (a,b,c) (x,y) -> (a+x, b+y, c+1) ) (0,0,0)
    System.DateTime.Now - startTime |> printfn "Execution Time: %O"
    printfn "part 1 fuel total: %i" part1
    printfn "part 2 fuel total: %i" part2
    printfn "%i records processed" count
    0
