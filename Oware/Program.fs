module Oware

type StartingPosition =
    | South
    | North

type state =
|Turn
|SouthWon
|NorthWon

type BoardState = {
player: StartingPosition
board: (int*int*int*int*int*int*int*int*int*int*int*int)
score: (int*int)
gamestatus: state
}

let getSeeds n board = failwith "Not implemented"

let useHouse n board = failwith "Not implemented"

let start position = 
    match position with
    |South -> {player=position; score=(0,0); board=(4,4,4,4,4,4,4,4,4,4,4,4); gamestatus = Turn}
    |North -> {player=position; score=(0,0); board=(4,4,4,4,4,4,4,4,4,4,4,4); gamestatus = Turn}

let score board = failwith "Not implemented"

let gameState board = failwith "Not implemented"

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
