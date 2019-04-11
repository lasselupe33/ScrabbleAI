open System.IO

open ScrabbleServer
open ScrabbleUtil.ServerCommunication
open utils
open console
open EvaluateScore
open console.State
open console
open console.Utils
open console.AI
open WordFinder

let recv play st msg =
    match msg with
    | RCM (CMPlaySuccess(ms, points, newPieces)) ->
        (* Successful play by you. Update your state *)
        let handWithPlayedPiecesRemoved = removeUsedPiecesFromHand st.hand ms

        let updatedMoves = addMovesToMap st.lettersPlaced ms
        let updatedHand = addPiecesToHand handWithPlayedPiecesRemoved newPieces

        let st' = mkState (st.ownPoints + points) updatedMoves updatedHand st.currentPlayerId st.playerList
        play st'
    | RCM (CMPlayed (pid, ms, points)) ->
        (* Successful play by other player. Update your state *)
        let updatedMoves = addMovesToMap st.lettersPlaced ms
        let st' = mkState st.ownPoints updatedMoves st.hand st.currentPlayerId st.playerList // This state needs to be updated
        play st'
    | RCM (CMPlayFailed (pid, ms)) ->
        (* Failed play. Update your state *)
        let st' = st // This state needs to be updated
        play st'
    | RCM (CMGameOver _) -> ()
    | RCM a -> failwith (sprintf "not implmented: %A" a)
    | RErr err -> printfn "Server Error:\n%A" err; play st
    | RGPE err -> printfn "Gameplay Error:\n%A" err; play st

let playGame send board pieces st =

    let rec aux st =
        Print.printBoard board 8 (State.lettersPlaced st)
        printfn "\n\n"
        Print.printHand pieces (State.hand st)

        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let hand = convertHandToCharList st.hand
        let validWords = getValidWords hand
        stopWatch.Stop();
         
        printfn "Input move (format '(<x-coordinate><y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)"
        let input =  System.Console.ReadLine()
        let move = RegEx.parseMove input

        send (recv aux st) (SMPlay move)

    aux st



let startGame send (msg : Response) = 
    match msg with
    | RCM (CMGameStarted (board, pieces, playerNumber, hand, playerList)) ->
        State.pieces <- pieces;
        let hand' = List.fold (fun acc (v, x) -> MultiSet.add v x acc) MultiSet.empty hand
        playGame send board pieces (State.newState hand' playerNumber playerList)
    | _ -> failwith "No game has been started yet"
     
[<EntryPoint>]
let main argv =
    printfn "%A" (calculatePoints [tripleLetterScore; singleLetterScore; doubleWordScore] [|('Q', 10); ('I', 1); ('N', 1)|])

    let send = Comm.connect ()
    send (startGame send) (SMStartGame(1u, "My game", "", "My name"))
    0 // return an integer exit code
