let run (arr: int []) =
    let update pos op =
        let a = arr.[pos+1]
        let b = arr.[pos+2]
        let c = arr.[pos+3] 
        op arr.[a] arr.[b] |> Array.set arr c

    let rec step pos =
        match arr.[pos] with
        | 1 -> 
            update pos (+)
            step (pos+4)
        | 2 ->
            update pos (*)
            step (pos+4)
        | 99 -> arr
        | x -> x |> sprintf "invalid opCode: %i" |> failwith
    step 0

let findInputs arr output =
    let rec run2 a b array2 =
        let array3 = Array.copy array2
        Array.set array3 1 a
        Array.set array3 2 b
        if (run array3).[0] = output then
            a*100 + b
        else
            if b = 100 then
                run2 (a+1) 0 array2
            else
                run2 a (b+1) array2
    run2 0 0 arr

[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let intCode =
        let a =
            Util.File.readLocalInputFile ()
            |> Seq.head
            |> (fun (s:string) -> s.Split([|','|]))
            |> Array.map int
        Array.set a 1 12
        Array.set a 2 2
        a
    let part1 = intCode |> Array.copy |> run |> Array.head
    System.DateTime.Now - startTime |> printfn "part 1 execution time: %O"

    let startTime2 = System.DateTime.Now
    let part2 = findInputs intCode 19690720
    System.DateTime.Now - startTime2 |> printfn "part 2 execution time: %O"
    printfn "part 1 solution: %i" part1
    printfn "part 2 solution: %i" part2
    0
