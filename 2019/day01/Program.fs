let fuelCalc x = x/3 - 2
let rec totalFuel baseMass newMass =
    match fuelCalc newMass with
    | x when x <= 0 -> baseMass
    | x -> totalFuel (baseMass + x) x
 
[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let (part1, part2, count) =
        Util.File.readLocalInputFile ()
        |> Seq.map (int >> (fun x -> (fuelCalc x, totalFuel 0 x, 1)))
        |> Seq.reduce (fun (a,b,c) (x,y,z) -> (a+x, b+y, c+z))
    System.DateTime.Now - startTime |> printfn "Execution Time: %O"
    printfn "%i records processed" count
    printfn "part 1 fuel total: %i" part1
    printfn "part 2 fuel total: %i" part2
    0
