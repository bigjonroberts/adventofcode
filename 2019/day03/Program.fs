
type GridPoint = int * int * int
type WirePath = Map<int*int,int list>
type WireTrace = {
    LeadPoint: GridPoint
    Path: WirePath }

let processCmd wireTrace (cmd:char seq) =
    match List.ofSeq cmd with
    | direction::distance ->
        let d = distance |> System.String.Concat |> int
        let nextMove =
            let (x,y,i) = wireTrace.LeadPoint
            match direction with
            | 'R' -> (fun m -> (x+m,y,i+m))
            | 'U' -> (fun m -> (x,y+m,i+m))
            | 'L' -> (fun m -> (x-m,y,i+m))
            | 'D' -> (fun m -> (x,y-m,i+m))
            | e -> e |> sprintf "'%c' is not a valid direction" |> failwith
            |> fun f -> seq { 1..d } |> Seq.map f
        let nextPath =
            let p = wireTrace.Path
            nextMove
            |> Seq.fold
                (fun p (x,y,i) ->
                    let i3 = Map.tryFind (x,y) p |> Option.defaultValue List.empty
                    Map.add (x,y) (List.append i3 [i]) p)
                wireTrace.Path
        { LeadPoint = Seq.last nextMove; Path = nextPath }
    | [] ->
        wireTrace
            

let getWirePath (s:string) =
    s.Split([|','|])
    |> Array.fold processCmd { LeadPoint = (0,0,0); Path = Map.empty }

[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let startTime2 = System.DateTime.Now
    let a =
        Util.File.readLocalInputFile ()
        |> Seq.map getWirePath
        |> Array.ofSeq
    printfn "wirepaths calculated"
    let path1 = a.[0].Path
    let path2 = a.[1].Path
    path1 |> Map.toSeq |> Seq.length |> printfn "Wire one has length %i"
    path2 |> Map.toSeq |> Seq.length |> printfn "Wire two has length %i"
    let intersections =
        path1
        |> Map.toArray
        |> Array.choose (fun ((x,y),xs) ->
            let p1Steps = List.min xs
            Map.tryFind (x,y) path2
            |> Option.map (fun p2 -> (x,y,List.min p2 + p1Steps)))
    intersections |> Array.length |> printfn "%i intersections found"
    intersections |> Array.iter (printfn "%O")
    let part1 =
        intersections
        |> Array.map (fun (x,y,_) -> (abs x) + (abs y))
        |> Array.min
    System.DateTime.Now - startTime |> printfn "part 1 execution time: %O"
    let part2 =
        intersections
        |> Array.map (fun (_,_,x) -> x)
        |> Array.min
    System.DateTime.Now - startTime2 |> printfn "part 2 execution time: %O"
    printfn "part 1 solution: %i" part1
    printfn "part 2 solution: %i" part2
    0
