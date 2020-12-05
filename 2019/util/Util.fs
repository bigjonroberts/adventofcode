module Util.File

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let readLocalInputFile () =
    [|  System.IO.Directory.GetCurrentDirectory (); "input" |]
    |> System.IO.Path.Combine
    |> readLines
