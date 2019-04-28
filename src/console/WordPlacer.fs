namespace console

open ScrabbleUtil
open utils.MultiSet
open WordFinder

module WordPlacer =
    type Direction = Up | Down | Left | Right

    let getAllWordPositions (board: board) (startCoord: coord) (possibleWords: (char * int) list list) = //: (Map<coord, char * int> list list) =
        let rec insertCoordToLetters (word: (char * int) list) (startCoord: coord) (direction: Direction) index (acc: Map<coord, char * int>) =
            match word with
                | [] -> acc
                | x::xs -> insertCoordToLetters xs startCoord direction (index + 1) (Map.add (fst startCoord + 1, snd startCoord) x acc)

        let singleWord = possibleWords.Head;
        let allPositionsInOneDirection = 0

        insertCoordToLetters possibleWords.Head startCoord Right 0 Map.empty

    let canPlaceWordOnBoard board word =
        true

    let getValidWordPositions (moves: Map<ScrabbleUtil.coord, char * int>) (board: board) (hand: MultiSet<uint32>) isValidWord pieces =
        let parsedHand = convertHandToPieceList hand pieces

        if moves.IsEmpty then
            let possibleWords = collectWords parsedHand [] isValidWord
            let placeableWords = List.filter (canPlaceWordOnBoard board) possibleWords
            0
        else    
            0


