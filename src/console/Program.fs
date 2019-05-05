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
open utils.MultiSet

let playGame cstream board pieces (st : State.state) isWordValid (timeout: uint32 option) =
    let rec aux (st : State.state) =
        if st.ownId = 1u then
            board.print (State.lettersPlaced st)
            printfn "\n\n"

        // Retrieve best word if our turn
        if st.currentPlayerId = st.ownId then
            let result = getBestWord st.lettersPlaced board st.hand isWordValid pieces timeout st.turn

            // If one exists then play it, else get a new hand
            match result with
                | Some r ->
                    let input = playMove pieces st.lettersPlaced (snd r)
                    let move = RegEx.parseMove input
                    send cstream (SMPlay move)
                | None ->
                    if st.tilesLeft > 6u then
                        send cstream (SMChange (toList st.hand))
                    else
                        send cstream (SMPass)

        let msg = recv cstream
        match msg with
        | RCM (CMPlaySuccess(ms, points, newPieces)) ->
            printfn "Success!!!\n Total points: %A\n Moves left: %A" (st.ownPoints + points) (st.tilesLeft - (min st.tilesLeft (uint32 ms.Length)))

            (* Successful play by you. Update your state *)
            let handWithPlayedPiecesRemoved = removeUsedPiecesFromHand st.hand ms

            let updatedMoves = addMovesToMap st.lettersPlaced ms
            let updatedHand = addPiecesToHand handWithPlayedPiecesRemoved newPieces

            let st' = mkState (st.ownPoints + points) updatedMoves updatedHand (getNextPlayerId st.currentPlayerId st) st.ownId st.playerList (st.tilesLeft - (min st.tilesLeft (uint32 ms.Length))) (st.turn + 1)
            aux st'
        | RCM (CMPlayed (pid, ms, points)) ->
            printfn "Player with id '%A' played a word for a total of %A points.\n\n" pid points
            let updatedMoves = addMovesToMap st.lettersPlaced ms
            let st' = mkState st.ownPoints updatedMoves st.hand (getNextPlayerId pid st) st.ownId st.playerList (st.tilesLeft - (min st.tilesLeft (uint32 ms.Length))) (st.turn + 1)
            aux st'
        | RCM (CMPlayFailed (pid, ms)) ->
            printfn "Player with id '%A' made a faulty move.\n\n" pid
            let st' = mkState st.ownPoints st.lettersPlaced st.hand (getNextPlayerId pid st) st.ownId st.playerList st.tilesLeft st.turn
            aux st'
        | RCM (CMPassed pid) ->
            printfn "Player with id '%A' passed.\n\n" pid
            let st' = mkState st.ownPoints st.lettersPlaced st.hand (getNextPlayerId pid st) st.ownId st.playerList st.tilesLeft st.turn
            aux st'
        | RCM (CMTimeout pid) ->
            printfn "Player with '%A' timed out.\n\n" pid
            let st' = mkState st.ownPoints st.lettersPlaced st.hand (getNextPlayerId pid st) st.ownId st.playerList st.tilesLeft st.turn
            aux st'
        | RCM (CMForfeit pid) ->
            printfn "Player with id '%A' forfeited.\n\n" pid
            let newPlayerList = List.filter (fun player -> fst player <> pid) st.playerList
            let currPlayer = if st.currentPlayerId = pid then getNextPlayerId pid st else st.currentPlayerId
            let st' = mkState st.ownPoints st.lettersPlaced st.hand currPlayer st.ownId newPlayerList st.tilesLeft st.turn
            aux st'
        | RCM (CMChangeSuccess newHand) ->
            printfn "We changed our hand, w00."
            let updatedHand = (addPiecesToHand MultiSet.empty newHand)
            let st' = mkState st.ownPoints st.lettersPlaced updatedHand (getNextPlayerId st.ownId st) st.ownId st.playerList st.tilesLeft st.turn
            aux st'
        | RCM (CMChange (playerId, changedHand)) ->
            printfn "Player with id '%A' changed their hand.\n\n" playerId
            let st' = mkState st.ownPoints st.lettersPlaced st.hand (getNextPlayerId st.ownId st) st.ownId st.playerList st.tilesLeft st.turn
            aux st'
        | RCM (CMGameOver finalScores) ->
            List.map (fun score -> printf "Player with '%A' scored %A points.\n" (fst score) (snd score)) finalScores
        | RCM a -> failwith (sprintf "not implmented: %A" a)
        | RErr err -> printfn "Server Error:\n%A" err; aux st
        | RGPE err ->
            printfn "Gameplay Error:\n%A" err;
            let st' = mkState st.ownPoints st.lettersPlaced st.hand (getNextPlayerId st.currentPlayerId st) st.ownId st.playerList st.tilesLeft st.turn
            aux st'


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
            printfn "\n\n Your id is: %A" playerNumber
            let handSet = List.fold (fun acc (x, k) -> MultiSet.add x k acc) MultiSet.empty hand
            playGame cstream board pieces (State.newState handSet firstPlayer playerNumber players numberOfPieces) isWordValid timeout
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
        let seed = Some 19

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
    [Comm.startServer 13000; startGame 13000 2u] |>
    Async.Parallel |>
    Async.RunSynchronously |> ignore
    0 // return an integer exit code
