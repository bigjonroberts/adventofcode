
let readLines (filePath:string) = seq {
    use sr = new System.IO.StreamReader (filePath)
    while not sr.EndOfStream do
        yield sr.ReadLine () }

type Facing =
    | North
    | East
    | South
    | West

type Command =
    | Forward of int
    | Left of int
    | Right of int
    | Move of Facing * int

type State = {
    Facing: Facing
    Position: int * int
}

let parse (line: char seq) =
    let toInt (xs: char list) = xs |> System.String.Concat |> int
    match List.ofSeq line with
    | 'F' :: num -> num |> toInt |> Forward
    | 'L' :: num -> num |> toInt |> Left
    | 'R' :: num -> num |> toInt |> Right
    | 'N' :: num -> Move (North, toInt num)
    | 'E' :: num -> Move (East, toInt num)
    | 'S' :: num -> Move (South, toInt num)
    | 'W' :: num -> Move (West, toInt num)
    | s -> s |> System.String.Concat |> failwithf "'%s' is not valid"

let move (state: State) (command: Command) =
    let (x,y) = state.Position
    match (command, state.Facing) with
    | (Forward n, North)
    | (Move (North, n), _) -> { state with Position = (x, y + n) }
    | (Forward n, East)
    | (Move (East, n), _) -> { state with Position = (x + n, y) }
    | (Forward n, South)
    | (Move (South, n), _) -> { state with Position = (x, y - n) }
    | (Forward n, West)
    | (Move (West, n), _) -> { state with Position = (x - n, y) }
    | (Left  90, East)  | (Left  180, South) | (Left  270, West)
    | (Right 90, West)  | (Right 180, South) | (Right 270, East) ->
        { state with Facing = North }
    | (Left  90, South) | (Left  180, West)  | (Left  270, North)
    | (Right 90, North) | (Right 180, West)  | (Right 270, South)
        -> { state with Facing = East }
    | (Left  90, West)  | (Left  180, North) | (Left  270, East)
    | (Right 90, East)  | (Right 180, North) | (Right 270, West)
        -> { state with Facing = South }
    | (Left  90, North) | (Left  180, East) | (Left  270, South)
    | (Right 90, South) | (Right 180, East) | (Right 270, North)
        -> { state with Facing = West }
    | x -> failwithf "Invalid command: %O" x

module Part2 =

    type State = {
        Position: int * int
        Waypoint: int * int
    }

    let move (state: State) (command: Command) =
        let (x, y) = state.Position
        let (wx, wy) = state.Waypoint
        match command with
        | Forward n ->       { state with Position = (x + n*wx, y + n*wy) }
        | Move (North, n) -> { state with Waypoint = (wx, wy + n) }
        | Move (East, n) ->  { state with Waypoint = (wx + n, wy) }
        | Move (South, n) -> { state with Waypoint = (wx, wy - n) }
        | Move (West, n) ->  { state with Waypoint = (wx - n, wy) }
        | Left  90 | Right 270 -> { state with Waypoint = (-1 * wy, wx) }
        | Left 180 | Right 180 -> { state with Waypoint = (-1 * wx, -1 * wy) }
        | Left 270 | Right  90 -> { state with Waypoint = (wy, -1 * wx) }
        | x -> failwithf "Invalid command: %O" x

[<EntryPoint>]
let main argv =

    let testinput =
        seq { "F10"; "N3"; "F7"; "R90"; "F11" }
        |> Seq.map parse
        |> Seq.cache        

    let input =
        "input"
        |> readLines
        |> Seq.map parse
        |> Seq.cache

    let outputResult part (x,y) =
        abs x + abs y
        |> printfn "Part %i: %i" part

    input
    |> Seq.fold move { Facing = East; Position = (0,0) }
    |> fun state -> state.Position
    |> outputResult 1

    input
    |> Seq.fold Part2.move { Position = (0,0); Waypoint = (10,1) }
    |> fun state -> state.Position
    |> outputResult 2

    0