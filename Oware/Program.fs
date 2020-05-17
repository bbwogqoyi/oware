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

(* The following function is intended to retrieve information about the number
   of seeds present within a given house, using a house number and the playing 
   field
   
   I will make use of the BoardState's board label defined in line 14 as the 
   playing field and bind the return integer value to an idenifying that will take
   the place of the given number of seeds in that house at that particular time.

   To achieve this, I will use a match expression with twelve patterns representing
   the game's twelve houses. To keep things simple, I will then bind the return value
   to an identifier starting from one through twelve (e.g. num1, num2, etc.).

   I had an error in referencing our board via BoardState.board - intelliSense flagged
   'board' as non static and since its not a literal, I couldn't use it in a pattern
   match. I then assigned it to a temporary identifier called playingMedium for the use
   in the pattern match. I then defined the return type as an integer. I am also taking
   cue from Siyanda's interface, labeling my houses like he did. 

   There aren't any tests pertaining to this function in its solitude. I hope it works.

   Please don't hesitate to give me any feedback and we can implement those changes.
 *)
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

let useHouse n board = failwith "Not implemented"

let start position = 
  {player=position; score=(0,0); board=(4,4,4,4,4,4,4,4,4,4,4,4); gamestatus = Turn}

let score boardState = boardState.score

let gameState board = 
    match South.score >= 25, North.score >= 25, (South.score = 24 && North.score = 24) with 
    |_, _, true -> "Game ended in a draw"
    |true, true, _ -> "Game ended in a draw"
    |true, false, _ -> "South won"
    |false, true, _ -> "North won"
    |false, false, _ -> match state.Turn with 
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
