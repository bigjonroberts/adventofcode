
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let selectHalf (minC: char, maxC: char) (minVal, maxVal) c =
    let diff = (maxVal - minVal + 1) / 2
    match c with
    | c when c = minC -> (minVal, maxVal - diff)
    | c when c = maxC -> (minVal + diff, maxVal)
    | _ -> (minVal, maxVal)

[<EntryPoint>]
let main argv =
    let seats =
        "input.txt"
        |> readLines
        |> Seq.map (fun pass ->
            let row =
                pass
                |> Seq.take 7
                |> Seq.fold (selectHalf ('F','B')) (0,127)
                |> fst
            let column =
                pass
                |> Seq.skip 7
                |> Seq.fold (selectHalf ('L','R')) (0,7)
                |> fst
            {| Row = row; Column = column; SeatID = (row * 8) + column |})
    let seatIds = 
        seats
        |> Seq.map (fun seat -> seat.SeatID)
    seatIds
    |> Seq.max
    |> printfn "Part 1: %i"

    let allSeatIds =
        seq { 1 .. 126 }
        |> Seq.collect (fun row ->
            seq { 0 .. 7 }
            |> Seq.map (fun column -> row * 8 + column))
    allSeatIds
    |> Seq.filter (fun seatId ->
        not (Seq.exists ((=) seatId) seatIds))
    |> Seq.find (fun seatId ->
        Seq.exists ((=) (seatId+1)) seatIds
            && Seq.exists ((=) (seatId-1)) seatIds)
    |> printfn "part 2: %i"

    0
