namespace console
open utils
open utils.MultiSet
open System.IO
open utils.Dictionary

module Utils =

    let addMovesToMap prevMovesMap ms = 
        List.fold (fun map move -> Map.add (fst move) (snd (snd move)) map) prevMovesMap ms

    let addPiecesToHand prevSet hand = List.fold (fun acc (v, x) -> MultiSet.add v x acc) prevSet hand

    let removePieceFromHand prevSet piece = MultiSet.removeSingle piece prevSet

    let removeUsedPiecesFromHand (prevSet: MultiSet<uint32>) (ms: (ScrabbleUtil.coord * (uint32 * (char * int))) list) : MultiSet<uint32> = 
        List.fold (fun set (_, move) -> removePieceFromHand set (fst move)) prevSet ms



    

    // Dictionary related stuff
    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let generateDict filePath = Seq.fold (fun dict word -> Dictionary.insert word dict) (Dictionary.empty "ABCDEFGHIJKLMNOPQRSTUVWXYZ") (readLines filePath)

    let scrabbleDict = generateDict "./EnglishDictionary.txt"

    let isWordValid word = Dictionary.lookup word scrabbleDict