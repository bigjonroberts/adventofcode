
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

type Bag = {
    Name: string
    Contains: Map<string,int>
}

let parseBagString (s: string) =
    let segments = s.Split(' ')
    (segments.[1] + " " + segments.[2], int segments.[0])

let parseChildren (s: string) =
    s
        .Remove(s.Length - 1, 1)
        .Replace("bags", "bag")
        .Split(", ")
    |> Array.map parseBagString
    |> Map.ofArray

let parse (line: string) =
    let segments = line.Split(" bags contain ")
    {   Name = segments.[0]
        Contains =
            match Seq.head segments.[1] with
            | 'n' -> Map.empty
            | _ -> parseChildren segments.[1] }

let canContain (bagMap: Map<string,Map<string,int>>) (bagColor: string) =
    let rec contains (parent: string) (child: string) =
        let hasChildren = Map.tryFind parent bagMap
        let isParentofChild =
            hasChildren
            |> Option.bind(fun childMap ->
                // printfn "checking children of %s" parent
                Map.tryFindKey (fun s _ -> s = child) childMap)
        match (isParentofChild, hasChildren) with
        | (Some _, _) -> true
        | (None, Some children) when Map.isEmpty children -> false
        | (None, Some children) ->
            children
            |> Map.map (fun p _ -> contains p bagColor)
            |> Map.toSeq
            |> Seq.map snd
            |> Seq.reduce (||)
        | _ -> false
    bagMap
    |> Map.filter (fun parent _ -> contains parent bagColor)
    |> Map.toSeq
    |> Seq.map fst

[<EntryPoint>]
let main argv =
    let bagMap =
        "input"
        |> readLines
        |> Seq.map ( parse >> (fun bag -> (bag.Name,bag.Contains)))
        |> Map.ofSeq
    canContain bagMap "shiny gold"
    |> Seq.length
    |> printfn "Part 1: %i"  
    0 // return an integer exit code
