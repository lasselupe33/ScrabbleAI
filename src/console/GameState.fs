namespace console
open ScrabbleUtil
open utils.MultiSet
open utils


module GameState =

    type OwnPieces = MultiSet<piece>

    let addPieces (pieces: (piece) list): OwnPieces = ofList pieces


    
