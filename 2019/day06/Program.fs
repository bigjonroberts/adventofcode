let parseObject (s:string) =
    let a = s.Split ([|')'|])
    (a.[1], a.[0])

let collectOrbits (objects:Map<string,string>) (objectName:string) =
    let rec traverse oName orbits =
        match Map.tryFind oName objects with
        | Some x -> traverse x (x::orbits)
        | None -> orbits
    traverse objectName List.empty

let countOrbits o name = collectOrbits o name |> List.length

[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let objects =
        Util.File.readLocalInputFile ()
        |> Seq.map parseObject
        |> Map.ofSeq
    let part1 =
        objects
        |> Map.toSeq
        |> Seq.sumBy (fun (k,_) -> countOrbits objects k)
    System.DateTime.Now - startTime |> printfn "part 1 execution time: %O"

    let startTime2 = System.DateTime.Now
    let orbits =
        objects
        |> Map.map (fun k _ -> countOrbits objects k)
        |> Map.add "COM" 0
    
    let mapOrbits oName =
        collectOrbits objects oName
        |> List.map (fun o -> (o, Map.find o orbits))
        |> Map.ofList

    let part2 =
        let youOrbits = mapOrbits "YOU"
        let sanOrbits = mapOrbits "SAN"
        let largestSharedOrbit =
            youOrbits
            |> Map.toSeq
            |> Seq.filter (fun (k,v) -> Map.exists (fun k' _ -> k' = k ) sanOrbits)
            |> Seq.sortBy snd
            |> Seq.last
            |> fst
        orbits.["YOU"]
        + orbits.["SAN"]
        - (2 * orbits.[largestSharedOrbit])
        - 2

        
    System.DateTime.Now - startTime2 |> printfn "part 2 execution time: %O"
    printfn "part 1 solution: %i" part1
    printfn "part 2 solution: %i" part2
    0
