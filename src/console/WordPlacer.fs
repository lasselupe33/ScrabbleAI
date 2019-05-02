namespace console

open ScrabbleUtil
open utils.MultiSet
open console.Utils
open WordFinder
open System

module WordPlacer =
    type Direction = Down | Right

    let getAdjDir dir = if dir = Down then Right else Down

    // Maps a direction, startCoord, a list of hardcoded chars and the current hand to a matching result
    let mutable cache: Map<(Direction * coord * ((char * int) * int) list * (char * int) list list), (coord * (char * int)) list list> = Map.empty

    let handSize = 7

    let writeLetter pieces (pieceLetter: (coord * (char * int))) : string =
        let coord = fst pieceLetter
        let letter = snd pieceLetter

        sprintf "%i %i %i%c%i" (fst coord) (snd coord) (getPieceId pieces (piece [letter])) (fst letter) (snd letter)


    let playMove pieces (lettersPlaced: Map<coord,(char * int)>) (word : (coord * (char * int)) list) : string =
        List.fold ( fun acc letter ->
            if Option.isNone(lettersPlaced.TryFind(fst letter))
                    then acc + " " + (writeLetter pieces (letter))
                    else "" + acc) "" word


    // Very simple helper that returns a new coordinate based on a direction and
    // an index to add to one of the directions
    let getNewCoord coord direction index =
        match direction with
        | Down -> (fst coord, snd coord + index)
        | Right -> (fst coord + index, snd coord)

    // Method that'll extract any pieces that are already placed on the board
    // in the direction we're specified with a given start coordinate,
    // which will be used to fully construct valid words that can be built in a
    // given direction.
    // Furthermore it also determines how long a word in the given direction can
    // possible by, based on holes in board, hardcoded characters and size of hand
    let extractBoardMetaInDirection (moves:  Map<ScrabbleUtil.coord, char * int>) (board: board) coord direction =
        let rec getRealStartCoord startCoord direction =
            let coordToCheck = getNewCoord startCoord direction (-1)

            if (moves.ContainsKey coordToCheck) then
                getRealStartCoord coordToCheck direction
            else
                startCoord

        // Helper that extracts and gets words that might be adjecent on a given
        // index
        let rec getAdjecentWord coord index adjecentDirection charsBefore charsAfter hasFoundCenter =
            let newCoord = getNewCoord coord adjecentDirection index

            if coord = newCoord then
                getAdjecentWord coord (index + 1) adjecentDirection charsBefore charsAfter true
            else
                let piece = moves.TryFind newCoord

                match piece with
                    | Some foundPiece ->
                        if not hasFoundCenter then
                            getAdjecentWord coord (index + 1) adjecentDirection (charsBefore @ [foundPiece]) charsAfter hasFoundCenter
                        else
                            getAdjecentWord coord (index + 1) adjecentDirection charsBefore (charsAfter @ [foundPiece]) hasFoundCenter
                    | None ->
                        if List.isEmpty charsBefore && List.isEmpty charsAfter then
                            None
                        else
                            Some (charsBefore, charsAfter)


        let rec aux index startCoord (hardcodedCharacters: ((char * int) * int) list) adjecentWords =
            if index = (handSize + hardcodedCharacters.Length) then
                (startCoord, index, hardcodedCharacters, adjecentWords)
            else
                let newCoord = getNewCoord startCoord direction index

                // Check if we've reached the end or a hole in the board
                if Option.isNone (board.tiles newCoord) then
                    (startCoord, index, hardcodedCharacters, adjecentWords)
                else
                    let newHardcoded =
                        match Map.tryFind newCoord moves with
                        | Some piece -> ((piece, index)::hardcodedCharacters)
                        | None -> hardcodedCharacters

                    let adjecentWord = getAdjecentWord newCoord 0 (getAdjDir direction) [] [] false

                    match adjecentWord with
                        | Some adjecentWord -> aux (index + 1) startCoord newHardcoded ((index, adjecentWord)::adjecentWords)
                        | None -> aux (index + 1) startCoord newHardcoded adjecentWords

        aux 0 (getRealStartCoord coord direction) [] []

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
        let getWordsInDirection startCoord direction minLength =
            let (realStartCoord, maxLength, hardcodedCharacters, adjecentWords) = extractBoardMetaInDirection moves board startCoord direction
            if Map.containsKey (direction, realStartCoord, hardcodedCharacters, hand) cache then
                Map.find (direction, realStartCoord, hardcodedCharacters, hand) cache
            else
                let possibleWordsInDirection = collectWords hand hardcodedCharacters isValidWord minLength maxLength
                let wordsInDirection = List.map (fun word -> insertCoordToLetters word realStartCoord direction) possibleWordsInDirection
                cache <- Map.add (direction, realStartCoord, hardcodedCharacters, hand) wordsInDirection cache
                wordsInDirection

        // Internal helper that collects all possibilities for words that hits
        // the start coordinate in some way in a given direction
        let checkAllPossibilitiesInDirection direction =
            let rec aux index acc =
                if index = handSize then
                    acc
                else
                    let newCoord = getNewCoord startCoord direction (index * -1)
                    aux (index + 1) ((getWordsInDirection newCoord direction (index + 1))::acc)

            aux 0 []

        (flatten (checkAllPossibilitiesInDirection Down)) @ (flatten (checkAllPossibilitiesInDirection Right))


