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
    0;; // return an integer exit code



