let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let readLocalInputFile () =
    [|  System.IO.Directory.GetCurrentDirectory (); "input" |]
    |> System.IO.Path.Combine
    |> readLines

type Source =
    | Value of int
    | Wire of string

type Instruction =
    | Not of NotInstruction
    | Assign of AssignInstruction
    | And of AndInstruction
  and NotInstruction = {
      Source: string
      Target: string }
  and AssignInstruction = {
      Value: int
      Target: string }
  and AndInstruction = {
      Source: Source
      Target: string
  }


// let getVals (s:string) = s.Split(' ') |> Array.ofSeq 

[<EntryPoint>]
let main _ =

    readLocalInputFile ()
    |> Seq.map(fun s ->
        let vals = s.Split(' ') |> Array.ofSeq
        match s |> Seq.filter (int >> (fun i -> 65 <= i && i <= 90)) |> String.concat "" with
        | "NOT" -> Not { Source = vals.[1]; Target = vals.[3] }
        | "" -> Assign { Value = int vals.[0]; Target = vals.[2] }
        | "AND" -> 
            let source =
                match System.Int32.TryParse vals.[0] with
                | true, x -> Value x
                | false, _ -> Wire vals.[0]
            And { Source = source; Target = vals.[2] }
        )
    |> ignore     

    0
