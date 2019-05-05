namespace console
open utils
open utils.MultiSet
open System.IO
open utils.Dictionary
open console.State

module Utils =

    let addMovesToMap prevMovesMap ms =
        List.fold (fun map move -> Map.add (fst move) (snd (snd move)) map) prevMovesMap ms

    let addPiecesToHand prevSet hand = List.fold (fun acc (v, x) -> MultiSet.add v x acc) prevSet hand

    let removePieceFromHand prevSet piece = MultiSet.removeSingle piece prevSet

    let removeUsedPiecesFromHand (prevSet: MultiSet<uint32>) (ms: (ScrabbleUtil.coord * (uint32 * (char * int))) list) : MultiSet<uint32> =
        List.fold (fun set (_, move) -> removePieceFromHand set (fst move)) prevSet ms

    let flatten list =
        List.reduce (@) list


    let getNextPlayerId currPlayerId state = fst state.playerList.[((List.findIndex (fun elm -> currPlayerId = fst elm) state.playerList) + 1) % state.playerList.Length]
