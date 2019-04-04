open ScrabbleUtil.Tile
open ScrabbleUtil.Board
open ScrabbleUtil.Coord
open ScrabbleUtil.Piece
open ScrabbleUtil.ServerCommunication
open ScrabbleUtil.StandardBoard
open utils.MultiSet
open EvaluateScore
open System
open ScrabbleUtil

[<EntryPoint>]
let main argv =
    utils.Say.hello argv.[0]

    let board = StandardBoard.mkStandardBoard()

    let fuck = 0
    printfn "%A" (calculatePoints [tripleLetterScore; singleLetterScore; doubleWordScore] [|('Q', 10); ('I', 1); ('N', 1)|])

    0;; // return an integer exit code