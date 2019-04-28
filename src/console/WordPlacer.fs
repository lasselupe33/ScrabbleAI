namespace console

open ScrabbleUtil
open utils.MultiSet
open console.Utils
open WordFinder

module WordPlacer =
    type Direction = Down | Right

    let handSize = 7

    // Very simple helper that returns a new coordinate based on a direction and
    // an index to add to one of the directions
    let getNewCoord coord direction index =
        match direction with
        | Down -> (fst coord, snd coord + index)
        | Right -> (fst coord + index, snd coord + index)

    // Method that'll extract any pieces that are already placed on the board
    // in the direction we're specified with a given start coordinate,
    // which will be used to fully construct valid words that can be built in a
    // given direction.
    // Furthermore it also determines how long a word in the given direction can
    // possible by, based on holes in board, hardcoded characters and size of hand
    let extractBoardMetaInDirection (moves:  Map<ScrabbleUtil.coord, char * int>) (board: board) coord direction =
        let rec aux index (hardcodedCharacters: ((char * int) * int) list) =
            if index = (handSize + hardcodedCharacters.Length) then
                (index, hardcodedCharacters)
            else
                let newCoord = getNewCoord coord direction index

                // Check if we've reached the end or a hole in the board
                if Option.isNone (board.tiles newCoord) then
                    (index, hardcodedCharacters)
                else
                    let newHardcoded =
                        match Map.tryFind newCoord moves with
                        | Some piece -> ((piece, index)::hardcodedCharacters)
                        | None -> hardcodedCharacters

                    aux (index + 1) newHardcoded

        aux 0 []

    // Helper that'll take a word (list of chars with points) and a start coordinate
    // and then add corresponding coordinates to the pieces based on direction.
    let insertCoordToLetters (word: (char * int) list) (coord: coord) (direction: Direction) =
        let rec aux (letters: (char * int) list) index (acc: (coord * (char * int)) list) =
            match letters with
                | [] -> acc
                | x::xs ->
                    let newCoord = getNewCoord coord direction index
                    aux xs (index + 1) (acc @ [(newCoord, x)])

        aux word 0 List.Empty

    // Method that'll return all possible positions that a list of words can be
    // placed upon based on a given startCoordinate.
    let getAllWordPositions (moves) (board: board) (startCoord: coord) (hand: (char * int) list list) isValidWord =
        // Internal helper that extracts all possible positions a single word can
        // remain within
        let getWordsInDirection startCoord direction =
            let (maxLength, hardcodedCharacters) = extractBoardMetaInDirection moves board startCoord direction
            let possibleWordsInDirection = collectWords hand hardcodedCharacters isValidWord
            List.map (fun word -> insertCoordToLetters word startCoord direction) possibleWordsInDirection


        let checkAllPossibilitiesInDirection direction =
            let rec aux index acc =
                if index = handSize then
                    acc
                else
                    let newCoord = getNewCoord startCoord direction (index * -1)
                    (getWordsInDirection newCoord direction)::acc

            aux 0 []

        (flatten (checkAllPossibilitiesInDirection Down)) @ (flatten (checkAllPossibilitiesInDirection Right))


    // Method that verifies whether or not a word is valid based on the letters
    // that is in the opposite direction of the word itself
    let canPlaceWordOnBoard board word =
        true

    // Method that returns ALL valid words and their positions based on the current
    // state of the board
    let getValidWordPositions (moves: Map<ScrabbleUtil.coord, char * int>) (board: board) (hand: MultiSet<uint32>) isValidWord pieces =
        let parsedHand = convertHandToPieceList hand pieces

        if moves.IsEmpty then
            getAllWordPositions moves board board.center parsedHand isValidWord
        else
            Map.toList moves |> List.map (fun move -> getAllWordPositions moves board (fst move) parsedHand isValidWord) |> flatten


