module Oware

open System

type StartingPosition =
| South
| North

type Movement =
| Clockwise 
| CounterClockwise

type State =
| Play
| Draw
| Win of StartingPosition


type BoardState = {
  player: StartingPosition
  board: (int*int*int*int*int*int*int*int*int*int*int*int)
  score: (int*int)

  gamestatus: state
}

let _collectSeeds selectedhouse boardState =
  match selectedhouse, boardState.board with
  | 1, (a,b,c,d,e,f,a',b',c',d',e',f')  -> a, (0,b,c,d,e,f,a',b',c',d',e',f')
  | 2, (a,b,c,d,e,f,a',b',c',d',e',f')  -> b, (a,0,c,d,e,f,a',b',c',d',e',f') 
  | 3, (a,b,c,d,e,f,a',b',c',d',e',f')  -> c, (a,b,0,d,e,f,a',b',c',d',e',f') 
  | 4, (a,b,c,d,e,f,a',b',c',d',e',f')  -> d, (a,b,c,0,e,f,a',b',c',d',e',f')
  | 5, (a,b,c,d,e,f,a',b',c',d',e',f')  -> e, (a,b,c,d,0,f,a',b',c',d',e',f')
  | 6, (a,b,c,d,e,f,a',b',c',d',e',f')  -> f, (a,b,c,d,e,0,a',b',c',d',e',f')
  | 7, (a,b,c,d,e,f,a',b',c',d',e',f')  -> a', (a,b,c,d,e,f,0,b',c',d',e',f')
  | 8, (a,b,c,d,e,f,a',b',c',d',e',f')  -> b', (a,b,c,d,e,f,a',0,c',d',e',f')
  | 9, (a,b,c,d,e,f,a',b',c',d',e',f')  -> c', (a,b,c,d,e,f,a',b',0,d',e',f')
  | 10, (a,b,c,d,e,f,a',b',c',d',e',f')  -> d', (a,b,c,d,e,f,a',b',c',0,e',f')
  | 11, (a,b,c,d,e,f,a',b',c',d',e',f')  -> e', (a,b,c,d,e,f,a',b',c',d',0,f')
  | 12, (a,b,c,d,e,f,a',b',c',d',e',f')  -> f', (a,b,c,d,e,f,a',b',c',d',e',0)
  | _ -> failwith "index is out-of-bound"

let _addSeed house boardState =
  match house, boardState with
  | 01, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a+1,b,c,d,e,f,a',b',c',d',e',f')
  | 02, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b+1,c,d,e,f,a',b',c',d',e',f') 
  | 03, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c+1,d,e,f,a',b',c',d',e',f') 
  | 04, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d+1,e,f,a',b',c',d',e',f')
  | 05, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e+1,f,a',b',c',d',e',f')
  | 06, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f+1,a',b',c',d',e',f')
  | 07, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a'+1,b',c',d',e',f')
  | 08, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b'+1,c',d',e',f')
  | 09, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c'+1,d',e',f')
  | 10, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d'+1,e',f')
  | 11, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d',e'+1,f')
  | 12, (a,b,c,d,e,f,a',b',c',d',e',f')  -> (a,b,c,d,e,f,a',b',c',d',e',f'+1)
  | _ -> failwith "index is out-of-bound"

let getSeeds houseNumber {BoardState.board = playingMedium} : int = 
    match houseNumber, playingMedium with
    | 1, (num1,_,_,_,_,_,_,_,_,_,_,_) -> num1
    | 2, (_,num2,_,_,_,_,_,_,_,_,_,_) -> num2
    | 3, (_,_,num3,_,_,_,_,_,_,_,_,_) -> num3
    | 4, (_,_,_,num4,_,_,_,_,_,_,_,_) -> num4
    | 5, (_,_,_,_,num5,_,_,_,_,_,_,_) -> num5
    | 6, (_,_,_,_,_,num6,_,_,_,_,_,_) -> num6
    | 7, (_,_,_,_,_,_,num7,_,_,_,_,_) -> num7
    | 8, (_,_,_,_,_,_,_,num8,_,_,_,_) -> num8
    | 9, (_,_,_,_,_,_,_,_,num9,_,_,_) -> num9
    | 10, (_,_,_,_,_,_,_,_,_,num10,_,_) -> num10
    | 11, (_,_,_,_,_,_,_,_,_,_,num11,_) -> num11
    | 12, (_,_,_,_,_,_,_,_,_,_,_,num12) -> num12
    | _ -> failwith "Not implemented"

//This function is defined to rotate the house numbers in a circular formation from 1 through 12
let _getFollowingHouse (op:Movement) houseNumber =
  let value = 
    match op with
    | Clockwise -> houseNumber+1
    | CounterClockwise -> houseNumber-1
  match value>12, value<1 with 
  | true, _ -> 1
  | _, true -> 12
  | _,_ -> value

let isValidHouseSelection selectedhouse boardState  = 
  // To validate if the house belongs to player 
  let isHouseOwnedByPlayer = 
    match boardState.player with
    | South -> selectedhouse<7
    | North -> selectedhouse>=7

  //  Validate if selected house is not empty
  let isHouseEmpty = (getSeeds selectedhouse boardState) > 0

  isHouseOwnedByPlayer && isHouseEmpty

let _getPlayerHouseList (player:StartingPosition) = 
  match player with
  | South -> List.init 6 (fun x -> x+1)
  | North -> List.init 6 (fun x -> x+7) 

let _getOpponentsHouseList (current:StartingPosition) = 
  match current with 
  | South -> _getPlayerHouseList North
  | North -> _getPlayerHouseList South

let _isDraw (s, n) = s=n && s=24   

let _isValidMove gameState =
  let rec checkOnOpponent count houses = 
    match houses with
    | [] -> count
    | entry::rest -> 
      let numOfSeeds = getSeeds entry gameState
      checkOnOpponent (count+numOfSeeds) rest

  let opponentHouseList = _getOpponentsHouseList gameState.player
  
  let canOpponentPlay = (checkOnOpponent 0 opponentHouseList) > 0
  let isDraw = _isDraw gameState.score
  canOpponentPlay || isDraw

let _distributeSeeds donorHouse boardState =
  let rec helper receivinghouse numOfSeeds board = 
    match numOfSeeds>0, receivinghouse=donorHouse with
    | false,_ -> ((_getFollowingHouse CounterClockwise receivinghouse), board)
    | true, true ->
       helper (_getFollowingHouse Clockwise receivinghouse) (numOfSeeds) board
    | true, false -> 
      let newBoard = _addSeed receivinghouse board
      helper (_getFollowingHouse Clockwise receivinghouse) (numOfSeeds-1) newBoard

  let (houseSeeds, newBoard) = _collectSeeds donorHouse boardState
  helper (donorHouse+1) houseSeeds newBoard

let useHouse selectedhouse boardState = 
  let selectionValid = isValidHouseSelection selectedhouse boardState
  match selectionValid with 
  | false -> boardState // do nothing -- the selection is invalid
  | _ ->
    let (lastVistedHouse, newBoard) = _distributeSeeds selectedhouse boardState

    match ( _isValidMove { boardState with board=newBoard } ) with
    | false -> boardState
    | true ->
      // TODO
      // calculate new score, update boardState, and toggle/switch player
      boardState //temporary

let start position = 
  {player=position; score=(0,0); board=(4,4,4,4,4,4,4,4,4,4,4,4); gamestatus = Turn}

let score boardState = boardState.score

let start position = 
  { player=position; score=(0,0); board=(4,4,4,4,4,4,4,4,4,4,4,4); status=Play }

let score boardState = boardState.score

let gameState boardState = 
  let (sPts, nPts) = boardState.score
  match sPts >= 25, nPts >= 25, (sPts = 24 && nPts= 24) with 
  |_, _, true -> "Game ended in a draw"
  |true, true, _ -> "Game ended in a draw"
  |true, false, _ -> "South won"
  |false, true, _ -> "North won"
  |false, false, _ -> 
    match boardState.player with 
    |South -> "South's turn"
    |North -> "North's turn"

let updateConsole () =
    System.Console.Clear ()
    System.Console.WriteLine ("\n\t\t---------------------------------------------------------------\n\t\t\t\t\t " + "South's Turn " + "\n\t\t===============================================================")
    System.Console.WriteLine ("\t\t                            NORTH")
    System.Console.WriteLine ("\t\t--------------------------POINTS: 0 ----------------------------")
    System.Console.WriteLine ("\t\t ||  ||                                                 ||  ||")
    System.Console.WriteLine ("\t\t ||  ||  12 [4]\t11 [4]\t10 [4]\t9  [4]\t8  [4]\t7  [4]  ||  ||")
    System.Console.WriteLine ("\t\t=====||=================================================||=====")
    System.Console.WriteLine ("\t\t ||  ||  1  [4]\t2  [4]\t3  [4]\t4  [4]\t5  [4]\t6  [4]  ||  ||")
    System.Console.WriteLine ("\t\t ||  ||                                                 ||  ||")
    System.Console.WriteLine ("\t\t--------------------------POINTS: 0 ----------------------------")
    System.Console.WriteLine ("\t\t                           SOUTH")
    System.Console.WriteLine ("\t\t===============================================================\n\t\t\t\t \n\t\t---------------------------------------------------------------")
    ()
//updateConsole


let __getUserInput () = // impure
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


  getConsoleInput ()

[<EntryPoint>]
let main _ =
  let rec playing (game:BoardState) = 
    updateConsole () // First we print out the board to the console
    match game.status = State.Play with
    | false -> game // its either someone won the game or its draw
    | true -> 
      let input = __getUserInput ()
      playing (useHouse input game)
    
  // We start the game
  let game:BoardState = start South
  let finalGame  = playing game 

  // prints out 'Draw' or the Winner og the game
  printfn "%s" (gameState finalGame)
