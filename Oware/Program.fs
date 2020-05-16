module Oware

type StartingPosition =
    | South
    | North

let getSeeds n board = failwith "Not implemented"

let useHouse n board = failwith "Not implemented"

let start position = failwith "Not implemented"

let score board = failwith "Not implemented"

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

[<EntryPoint>]
let main _ =
    printfn "Hello from F#!"
    0 // return an integer exit code
