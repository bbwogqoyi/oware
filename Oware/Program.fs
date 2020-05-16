module Oware

open System

type StartingPosition =
| South
| North

type state =
| Turn
| SouthWon
| NorthWon

type BoardState = {
  player: StartingPosition
  board: (int*int*int*int*int*int*int*int*int*int*int*int)
  score: (int*int)
  gamestatus: state
}

let getSeeds n board = failwith "Not implemented"

let useHouse n board = failwith "Not implemented"

let start position = 
  {player=position; score=(0,0); board=(4,4,4,4,4,4,4,4,4,4,4,4); gamestatus = Turn}

let score boardState = boardState.score

let gameState board = failwith "Not implemented"

let __getUserInput game = // impure
  let rec getConsoleInput () = 
    let retry () = printfn "Invalid selection, try again" |> getConsoleInput
    let input = System.Console.ReadLine()
    match (not (String.IsNullOrWhiteSpace input)) && (String.forall System.Char.IsDigit input)  with
    | false -> retry ()
    | true -> 
      let selectedHouse = int input
      match selectedHouse>=1 && selectedHouse<=12 with
      | false -> retry ()
      | true -> selectedHouse

  printfn "%s\n" (gameState game)
  getConsoleInput ()

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
