namespace ScrabbleUtil

open System.IO

open MBrace.FsPickler
open MBrace.FsPickler.Json

open FSharp.Quotations
open FSharp.Quotations.Evaluator

module ServerCommunication =    
        
    type ServerMessage =
        | SMStartGame of uint32 * string * string * string
        | SMJoinGame  of uint32 * string * string
        | SMPlay      of (coord * (uint32 * (char * int))) list
        | SMPass      
        | SMForfeit
        | SMChange    of uint32 list

    type ServerError =
        | ErrInvalidMessage    of string
        | ErrInvalidPassword   of uint32
        | ErrGameDoesNotExist  of uint32  
        | ErrGameIsFull        of uint32 * uint32
        | ErrNoUserName    
        | ErrTooFewPieces      of uint32 * uint32 * uint32

    type ClientMessage =
        | CMPlayerJoined       of uint32 * string
        | CMGameStarted        of board * Map<uint32, piece> * uint32 * string * (uint32 * string) list
        | CMPlayed             of uint32 * (coord * (uint32 * (char * int))) list * int
        | CMPlayFailed         of uint32 * (coord * (uint32 * (char * int))) list
        | CMPassed             of uint32
        | CMForfeit            of uint32
        | CMChange             of uint32 * uint32
        | CMTimeout            of uint32

    type GameplayError =
    | GPEOccupiedTile of (char * int) * coord * (char * int)
    | GPEWordNotOnRowOrColumn of coord list
    | GPEEmptyMove
    | GPEInvalidPieceInst of uint32 * (char * int)
    | GPEPieceDoesNotExist of uint32
    | GPEEmptyTile of coord
    | GPEPlayerDoesNotHavePiece of uint32 * uint32
    | GPEWordNotConnected
    | GPEWordOutsideBoard
    | GPEWordNotInDictionary of string
    | GPEFirstWordNotOverCenter of coord
    | GPEFirstWordTooShort
    | GPEWordNotAdjacent

    type Response =
        | RCM  of ClientMessage
        | RErr of ServerError
        | RGPE of GameplayError list        


    let formatter = FsPickler.CreateBinarySerializer()

    let send (cstream : Stream) msg =
        formatter.Serialize(cstream, <@ msg @>, leaveOpen = true)

    let recv (cstream : Stream) =
        formatter.Deserialize<Expr<Response>>(cstream, leaveOpen = true) |>
        QuotationEvaluator.Evaluate

    let trecv<'T> (cstream : Stream) =
        formatter.Deserialize<Expr<'T>>(cstream, leaveOpen = true) |>
        QuotationEvaluator.Evaluate