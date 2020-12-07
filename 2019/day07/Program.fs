type ParamMode =
    | Position
    | Immediate

type ComplexOpCode = {
    OpCode: int
    ParamModes: ParamMode [] }

let parseOpCode x =
    let a' = Array.init 10 (fun _ -> Position)
    printfn "  opCode: %i" x
    if x < 99 then
        { OpCode = x
          ParamModes = a' }
    else
        { OpCode = x % 100
          ParamModes =
            string x
            |> Seq.toArray
            |> Array.rev
            |> Array.skip 2
            |> Array.map 
               (function
                | '0' -> Position
                | '1' -> Immediate
                | x -> x |> sprintf "invalid param mode: %c" |> failwith )
            |> Array.iteri (fun i x -> Array.set a' i x ) 
            a' }

let run phase input (arr: int []) =

    let getParam p v =
        match p with
        | Position -> arr.[v]
        | Immediate -> v

    let rec step (value: int option) pos =
        printfn "position: %i" pos
        let opCode = parseOpCode arr.[pos]
        printfn "  value: %O" value
        match opCode.OpCode with
        | 1 ->
            let a = getParam opCode.ParamModes.[0] arr.[pos+1]
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            let c = arr.[pos+3]
            let v = a + b
            printfn "    saving %i + %i = %i to position %i" a b v c
            Array.set arr c v
            step (Some v) (pos+4)
        | 2 ->
            let a = getParam opCode.ParamModes.[0] arr.[pos+1]
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            let c = arr.[pos+3]
            let v = a * b
            printfn "    saving %i * %i = %i to position %i" a b v c
            Array.set arr c v
            step (Some v) (pos+4)
        | 3 ->
            match value with
            | Some v ->
                printfn "    setting position %i to %i" (arr.[pos+1]) v 
                v |> Array.set arr arr.[pos+1]
                step value (pos+2)
            | None -> "opcode 3 expects a value" |> failwith
        | 4 ->
            let a = getParam opCode.ParamModes.[0] arr.[pos+1]
            printfn "\noutput: %i\n" a
            step (Some a) (pos+2)
        | 5 -> 
            match getParam opCode.ParamModes.[0] arr.[pos+1] with
            | 0 -> pos+3
            | _ -> getParam opCode.ParamModes.[1] arr.[pos+2]
            |> step value
        | 6 -> 
            match getParam opCode.ParamModes.[0] arr.[pos+1] with
            | 0 -> getParam opCode.ParamModes.[1] arr.[pos+2]
            | _ -> pos+3
            |> step value
        | 7 -> 
            let a = getParam opCode.ParamModes.[0] arr.[pos+1]
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            if a < b then 1 else 0
            |> Array.set arr arr.[pos+3]
            step value (pos+4)
        | 8 -> 
            let a = getParam opCode.ParamModes.[0] arr.[pos+1]
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            if a = b then 1 else 0
            |> Array.set arr arr.[pos+3]
            step value (pos+4)
        | 99 ->
            printfn "** DONE **"
            Option.defaultValue -1 value
        | x -> x |> sprintf "invalid opCode: %i" |> failwith
    step (Some input) phase

[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let intCode =
        Util.File.readLocalInputFile ()
        |> Seq.head
        |> (fun (s:string) -> s.Split([|','|]))
        |> Array.map int
    let runSimulation intCode (phaseSettings: int []) =
        let amplifier phase input = 
            printfn "running with phase '%i' and input '%i'" phase input
            intCode |> Array.copy |> run input phase
        amplifier phaseSettings.[0] 0
        |> amplifier phaseSettings.[1]
        |> amplifier phaseSettings.[2]
        |> amplifier phaseSettings.[3]
        |> amplifier phaseSettings.[4]
    let phaseValueSet () = seq { 0 .. 4 } |> Seq.toArray
    runSimulation
        ("3,15,3,16,1002,16,10,16,1,16,15,15,4,15,99,0,0" |> (fun s -> s.Split([|','|])) |> Array.map int)
        [|4;3;2;1;0|]
    |> printfn "test 4,3,2,1,0 = %i"
    let mutable results = List.empty<int> // Map.empty<int,int>
    phaseValueSet ()
    |> Array.iter(fun zero ->
        phaseValueSet ()
        |> Array.iter(fun one ->
            phaseValueSet ()
            |> Array.iter(fun two ->
                phaseValueSet ()
                |> Array.iter (fun three ->
                    phaseValueSet ()
                    |> Array.iter (fun four ->
                        let settings = [| zero; one; two; three; four |]
                        let settingsAsInt = zero*10000 + one*1000 + two*100 + three*10 + four
                        printfn "attempting %i" settingsAsInt
                        //results <- Map.add settingsAsInt (runSimulation settings) results)))))
                        results <- (runSimulation intCode settings)::results)))))

    let part1 = List.max results

    System.DateTime.Now - startTime |> printfn "part 1 execution time: %O"

    // let startTime2 = System.DateTime.Now
    // let part2 = intCode |> Array.copy |> run (Some 5)
    // System.DateTime.Now - startTime2 |> printfn "part 2 execution time: %O"
    printfn "part 1 solution: %i" part1
    // printfn "part 2 solution: %i" part2
    0
