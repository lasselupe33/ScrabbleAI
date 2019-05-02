open System.IO

open ScrabbleServer
open ScrabbleUtil
open ScrabbleUtil.ServerCommunication
open utils
open console
open EvaluateScore
open console.State
open console.AI
open console.Utils
open console.WordPlacer
open WordFinder

open System.Net.Sockets
open System

let playGame cstream board pieces (st : State.state) isWordValid =

    let rec aux (st : State.state) =
        board.print (State.lettersPlaced st)
        printfn "\n\n"
        Print.printHand pieces (State.hand st)

        // Retrieve best word
        let stopWatch = System.Diagnostics.Stopwatch.StartNew()
        let hand = convertHandToPieceList st.hand
        let result = getBestWord st.lettersPlaced board st.hand isWordValid pieces

        stopWatch.Stop();

        // // let input = playMove pieces st.lettersPlaced (snd result)
        // // let move = RegEx.parseMove input
        // printfn "Input move (format '(<x-coordinate><y-coordinate> <piece id><character><point-value> )*', note the absence of state between the last inputs)"
        // let input =  System.Console.ReadLine()

        // let move = RegEx.parseMove input

        // If one exists then play it, else get a new hand
        match result with
            | Some r ->
                let input = playMove pieces st.lettersPlaced (snd r)
                let move = RegEx.parseMove input
                printfn "Trying to play: %A" move
                send cstream (SMPlay move)
            | None -> send cstream (SMPlay (RegEx.parseMove "hej"))

        // send cstream (SMPlay move)
        let msg = recv cstream
        match msg with
        | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            printfn "Success!!!\n%A\n%A\n%A" ms points newPieces

            (* Successful play by you. Update your state *)
            let handWithPlayedPiecesRemoved = removeUsedPiecesFromHand st.hand ms

            let updatedMoves = addMovesToMap st.lettersPlaced ms
            let updatedHand = addPiecesToHand handWithPlayedPiecesRemoved newPieces

            let st' = mkState (st.ownPoints + points) updatedMoves updatedHand st.currentPlayerId st.playerList (st.tilesLeft - (uint32 ms.Length))
            aux st'
        | RCM (CMPlayed (pid, ms, points)) ->
            (* Successful play by other player. Update your state *)
            let updatedMoves = addMovesToMap st.lettersPlaced ms
            let st' = mkState st.ownPoints updatedMoves st.hand st.currentPlayerId st.playerList (st.tilesLeft - (uint32 ms.Length)) // This state needs to be updated
            aux st'
        | RCM (CMPlayFailed (pid, ms)) ->
            (* Failed play. Update your state *)
            let st' = st // This state needs to be updated
            aux st'
        | RCM (CMGameOver _) -> ()
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RErr err -> printfn "Server Error:\n%A" err; aux st
        | RGPE err -> printfn "Gameplay Error:\n%A" err; aux st


    aux st

let setupGame cstream board alphabet words handSize timeout =
    let rec aux () =
        match ServerCommunication.recv cstream with
        | RCM (CMPlayerJoined name) ->
            printfn "Player %s joined" name
            aux ()
        | RCM (CMGameStarted (playerNumber, hand, numberOfPieces, firstPlayer, pieces, players)) as msg ->
            // Setup function used to handle word validation checks
            let scrabbleDict = Seq.fold (fun dict word -> Dictionary.insert word dict) (Dictionary.empty alphabet) (words)
            let isWordValid word = Dictionary.lookup word scrabbleDict

            printfn "Game started %A" msg
            let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
            playGame cstream board pieces (State.newState handSet playerNumber players numberOfPieces) isWordValid
        | msg -> failwith (sprintf "Game initialisation failed. Unexpected message %A" msg)

    aux ()

let joinGame port gameId password playerName =
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        use cstream = client.GetStream()
        send cstream (SMJoinGame (gameId, password, playerName))

        match ServerCommunication.recv cstream with
            | RCM (CMJoinSuccess(board, numberOfPlayers, alphabet, words, handSize, timeout)) ->
                setupGame cstream board alphabet words handSize timeout
            | msg -> failwith (sprintf "Error joining game%A" msg)

    }

let startGame port numberOfPlayers =
    async {
        let client = new TcpClient(sprintf "%A" (localIP ()), port)
        let cstream = client.GetStream()
        let path = "../../../EnglishDictionary.txt"
        let words = File.ReadLines path |> Seq.toList
        let board = StandardBoard.mkStandardBoard ()
        //let board = InfiniteStandardBoard.mkBoard ()
        //Make a torus-shaped board with in outer diameter of 10 and an inner diameter of 3
        //let board = Torus.mkBoard 10 3
        let pieces = English.pieces 1u (* change the number to scale the number of pieces *)
        let alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        let handSize = 7u
        let timeout = None
        let seed = Some 5

        send cstream (SMStartGame (numberOfPlayers, "My game", "password", "My name", seed, board, pieces,
                                    handSize, alphabet, words, timeout))

        let gameId =
            match ServerCommunication.recv cstream with
            | RCM (CMGameInit gameId) -> gameId
            | msg -> failwith (sprintf "Error initialising game, server sent other message than CMGameInit (should not happen)\n%A" msg)

        do! (async { setupGame cstream board alphabet words handSize timeout } ::
             [for i in 2u..numberOfPlayers do yield joinGame port gameId "password" ("Player" + (string i))] |>
             Async.Parallel |> Async.Ignore)
    }

[<EntryPoint>]
let main argv =
    [Comm.startServer 13000; startGame 13000 1u] |>
    Async.Parallel |>
    Async.RunSynchronously |> ignore
    0 // return an integer exit code
