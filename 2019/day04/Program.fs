
let ``two adjacent digits are the same`` =
    string
    >> Seq.pairwise
    >> Seq.fold (fun same (a,b) -> same || a = b) false

let ``digits never decrease`` =
    string
    >> Seq.map int
    >> Seq.pairwise
    >> Seq.fold (fun increase (a,b) -> increase && a <= b) true

let ``only two in matching group`` x =
    string x
    |> Seq.toList
    |> List.fold
        (fun (foundPair,grp) c ->
            match (foundPair,grp,List.length grp) with
            | (true,[],_) -> (true,[]) 
            | (true,x::_,_) ->
                if x = c then
                    (false,c::grp)
                else
                    (true,[])
            | (false,[],2) -> (true,[])
            | (false,[],_) -> (false,[])
            | (false,x::_,1) ->
                if x = c then
                    (true,c::grp)
                else
                    (false,[c])
            | (false,x::_,2) ->
                if x = c then
                    (false,c::grp)
                else
                    (true,grp)
            | (false,x::_,_) ->
                if x = c then
                    (false,c::grp)
                else
                    (false,[c]))
        (false,[' '])
    |> fst            

[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let startTime2 = System.DateTime.Now
    let part1a =
        seq { 264360..746325 }
        |> Seq.filter (fun x ->
            ``two adjacent digits are the same`` x
            && ``digits never decrease`` x)
        |> Seq.cache
    let part1 = Seq.length part1a
    System.DateTime.Now - startTime |> printfn "part 1 execution time: %O"
    let part2 =
        part1a
        |> Seq.filter (``only two in matching group``)
        // |> Seq.map (fun x -> printfn "%O" x; x)
        |> Seq.length
    System.DateTime.Now - startTime2 |> printfn "part 2 execution time: %O"
    printfn "part 1 solution: %i" part1
    printfn "part 2 solution: %i" part2
    0
