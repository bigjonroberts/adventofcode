
type SeatState =
    | Occupied
    | Empty
    | Floor

type Seat = {
    State: SeatState
    Position: int * int
    Neighbors: (int * int) list
}

let parse c =
    match c with
    | 'L' -> Empty
    | '#' -> Occupied
    | '.' -> Floor
    | x -> failwithf "%c is not a valid seat state" x

let parseLine line =
    line
    |> Seq.map parse
    |> Seq.toArray

let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

let toSeat (input: SeatState [] []) height width x y _ =
    let neighbors =
        match (x,y) with
        | (0,0) -> [ (0,1); (1,0); (1,1) ]
        | (0,w) when w = width - 1 -> [ (0, w-1); (1, w-1); (1,w) ]
        | (h,0) when h = height - 1 -> [ (h-1, 0); (h-1, 1); (h,1) ]
        | (h,w) when h = height - 1 && w = width - 1 -> [ (h-1, w-1); (h-1, w); (h,w-1) ]
        | (0,c) -> [ (0,c-1); (0,c+1); (1,c-1); (1,c); (1,c+1) ]
        | (r,0) -> [ (r-1,0); (r-1,1); (r,1); (r+1,0); (r+1,1) ]
        | (r,c) when c = width - 1 -> [ (r-1,c-1); (r-1,c); (r,c-1); (r+1,c-1); (r+1,c) ]
        | (r,c) when r = height - 1 -> [ (r-1,c-1); (r-1,c); (r-1,c+1); (r,c-1); (r,c+1) ]
        | (r,c) -> [
           (r-1,c-1); (r-1,c); (r-1,c+1)
           (r,c-1);            (r,c+1)
           (r+1,c-1); (r+1,c); (r+1,c+1) ]
    { State = input.[x].[y]; Position = (x,y); Neighbors = neighbors }

let tryOccupy (seatsArray: Seat [,]) (getNeighbors: unit -> (int * int) list) (seat: Seat) =
    let allEmpty () =
        let neighbors = getNeighbors ()
        neighbors
        |> List.forall (fun (x,y) -> (Array2D.get seatsArray x y).State <> Occupied)
        |> (||) (List.isEmpty neighbors)
    if seat.State = Empty && allEmpty () then
        { seat with State = Occupied }
    else
        seat

let tryVacate maxOccupied (seatsArray: Seat [,]) (getNeighbors: unit -> (int * int) list) (seat: Seat) =
    let xOccupied () =
        getNeighbors ()
        |> List.filter (fun (x,y) -> (Array2D.get seatsArray x y).State = Occupied)
        |> List.length
        |> fun count -> count >= maxOccupied
    if seat.State = Occupied && xOccupied () then
        { seat with State = Empty }
    else
        seat

let mutate mutateSeat (seatsArray: Seat [,]) = async {
    let newArray = Array2D.copy seatsArray
    let! unitArray =
        seq { 0 .. Array2D.length1 seatsArray - 1 }
        |> Seq.map (fun r -> async {
            seatsArray.[r,*]
            |> Array.map (mutateSeat seatsArray)
            |> Array.iteri (fun c seat ->
                newArray.[r,c] <- seat)
            return () })
        |> Async.Parallel
    return newArray
}

let mutateSeat priorArray seat =
    seat
    |> tryVacate 4 priorArray (fun () -> seat.Neighbors)
    |> tryOccupy priorArray (fun () -> seat.Neighbors)

let printSeat seat =
    match seat.State with
    | Occupied -> '#'
    | Empty -> 'L'
    | Floor -> '.'
    |> printf "%c"

let printSeats (seatArray: Seat [,]) = 
    seq { 0 .. Array2D.length1 seatArray - 1 }
    |> Seq.iter (fun r ->
        seatArray.[r,*]
        |> Array.iter (printSeat)
        printfn "")
    printfn "\n"

let rec findStasis mutate seatsArray = async {
    let! newArray = mutate seatsArray
    if (newArray = seatsArray) then
        return
            seatsArray
            |> Seq.cast<Seat>
            |> Seq.filter (fun s -> s.State = Occupied)
            |> Seq.length
    else
        return! findStasis mutate newArray
}

module Part2 =

    let rec look (seatArray: Seat [,]) (startPos: int * int) (direction: int * int) =
        let (ydir, xdir) = direction
        let (y, x) = startPos
        let newY = y + ydir
        let newX = x + xdir
        // printfn "y: %i; x: %i" newY newX
        if (newY >= 0) && (newY <= Array2D.length1 seatArray - 1)
           && (newX >= 0) && (newX <= Array2D.length2 seatArray - 1) then
            if seatArray.[newY,newX].State <> Floor then
                Some (newY, newX)
            else
                look seatArray (newY, newX) direction
        else
            None

    let lookNeighbors (seatArray: Seat [,]) (position: int * int) =
        let look = look seatArray position
        [   look (-1,-1)
            look (-1,0)
            look (-1,1)
            look (0,-1)
            look (0,1)
            look (1,-1)
            look (1,0)
            look (1,1) ]

    let mutateSeat priorArray seat =
        seat
        |> tryOccupy priorArray (fun () -> lookNeighbors priorArray seat.Position |> (List.choose id))
        |> tryVacate 5 priorArray (fun () -> lookNeighbors priorArray seat.Position |> (List.choose id))


    // let mutate (seatsArray: Seat [,]) = async {
    //     let priorArray = Array2D.copy seatsArray
    //     return
    //         seatsArray
    //         |> Array2D.map (mutateSeat priorArray)
    // }

[<EntryPoint>]
let main argv =
    let seats =
        let input =
            "input"
            |> readLines
            |> Seq.map parseLine
            |> Array.ofSeq
        let height = input.Length
        let width = input.[0].Length

        Array2D.init height width

        Array2D.create<SeatState> height width Floor
        |> Array2D.mapi (toSeat input height width)
    
    [ (1, mutateSeat); (2, Part2.mutateSeat) ]
    |> Seq.map (fun (part, mutateSeat) -> async {
        let! result = findStasis (mutate mutateSeat) seats
        return (part, result) })
    |> Async.Parallel
    |> Async.RunSynchronously

    |> Array.iter (fun (part,result) -> printfn "Part %i: %i" part result)

    0
