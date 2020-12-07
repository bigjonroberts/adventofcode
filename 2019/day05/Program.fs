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

let run input (arr: int []) =

    let getParam p v =
        match p with
        | Position -> arr.[v]
        | Immediate -> v

    let rec step (value: int option) pos =
        printfn "position: %i" pos
        let opCode = parseOpCode arr.[pos]
        let a = getParam opCode.ParamModes.[0] arr.[pos+1]
        match opCode.OpCode with
        | 1 -> 
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            let c = arr.[pos+3]
            printfn "    saving %i + %i = %i to position %i" a b (a+b) c
            a + b |> Array.set arr c
            step None (pos+4)
        | 2 ->
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            printfn "    saving %i * %i = %i to position %i" a b (a+b) c
            a * b |> Array.set arr arr.[pos+3]
            step None (pos+4)
        | 3 ->
            match value with
            | Some v ->
                printfn "    setting position %i to %i" (arr.[pos+1]) v 
                v |> Array.set arr arr.[pos+1]
                step value (pos+2)
            | None -> "opcode 3 expects a value" |> failwith
        | 4 ->
            printfn "\noutput: %i\n" a
            step value (pos+2)
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
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            if a < b then 1 else 0
            |> Array.set arr arr.[pos+3]
            step value (pos+4)
        | 8 -> 
            let b = getParam opCode.ParamModes.[1] arr.[pos+2]
            if a = b then 1 else 0
            |> Array.set arr arr.[pos+3]
            step value (pos+4)
        | 99 -> Option.defaultValue -1 value
        | x -> x |> sprintf "invalid opCode: %i" |> failwith
    step input 0

[<EntryPoint>]
let main _ =
    let startTime = System.DateTime.Now
    let intCode =
        Util.File.readLocalInputFile ()
        |> Seq.head
        |> (fun (s:string) -> s.Split([|','|]))
        |> Array.map int
    let part1 = intCode |> Array.copy |> run (Some 1)
    System.DateTime.Now - startTime |> printfn "part 1 execution time: %O"

    let startTime2 = System.DateTime.Now
    let part2 = intCode |> Array.copy |> run (Some 5)
    System.DateTime.Now - startTime2 |> printfn "part 2 execution time: %O"
    printfn "part 1 solution: %i" part1
    printfn "part 2 solution: %i" part2
    0
