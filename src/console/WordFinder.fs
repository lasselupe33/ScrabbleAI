namespace console

open utils
open utils.MultiSet
open Utils
open ScrabbleUtil
open System.Collections.Generic

module WordFinder =
    // Simple helper that based on a pieceId extracts the Char that is related to
    // it.
    // Currently we've commented Logic out to use all pieces of the WildCard since
    // it makes our AI perform too slow. If you'd like to unleash the full potential
    // of the wildcards, simply comment in the line below
    //
    // let getPiece (pieces: Map<uint32, piece>) pieceId = (Set.toList (Map.find pieceId pieces))
    let getPiece (pieces: Map<uint32, piece>) pieceId = [(Set.toList (Map.find pieceId pieces)).Head]

    let getPieceId (pieces: Map<uint32, piece>) pieceValue =
        match Map.tryFindKey (fun _ piece -> piece = pieceValue) pieces with
            | Some key -> key
            | None -> 0u

    // Helper that converts the hand of piece identifiers to a list of characters
    let convertHandToPieceList (hand: MultiSet<uint32>) pieces = List.map (getPiece pieces) (MultiSet.toList hand)

    // Helper that returns a list of all possible start combinations of a list
    // e.g [a,b,c,d] -> [[a,b,c,d], [b,a,c,d]. [c,a,b,d], [d,a,b,c]]
    let getAllStarts (list: (char * int) list list) =
        let rec aux n result =
            let length = List.length list
            if (n > 0 && n < length) then
                aux (n - 1) ((List.permute (fun index -> (index + n) % length) list)::result)
            else
                list::result

        aux (list.Length - 1) List.empty

    let getPieceChar (piece: char * int) = fst piece
    let convertPiecesToString (pieces: (char * int) list) = List.fold (fun (acc: string) (piece: char * int)  -> acc + string (getPieceChar piece)) "" pieces

    // Helper to make a list consisting of merging the pieces of the hand and
    // the hardcoded letters from the board together to one list
    let mergePiecesAndHardcodedLetters  (pieces: (char * int) list)
                                        (hardcodedPieces: ((char * int) * int) list)
                                        (adjecentWords: (int * ((char * int) list * (char * int) list)) list) =

        let checkAdjecentWords index accx letter =
            List.fold (fun acc word -> if index = fst word then acc @ [(fst (snd word) @ [letter] @ (snd (snd word)))] else acc) accx adjecentWords

        // Internal helper that checks whether the passed index match with the
        // position of any hardcoded piece in the list, if there is, then append
        // the piece to the accumulator
        let checkCoordinates (xAcc: (char * int) list list) index =
            List.fold (fun acc (hcPiece: ((char * int) * int)) ->
                if (snd hcPiece) = index then ((acc.Head @ [(fst hcPiece)])::acc.Tail) else acc) xAcc hardcodedPieces

        // The recursive helper that
        let rec aux index (acc: (char * int) list list) (list: (char * int) list) =
            match list with
            | [] ->
                // Even though it has hit base case, there could still
                // be hardcoded pieces to add
                let newAcc = checkCoordinates acc index

                // If lenghts are the same...
                if newAcc.Head.Length = acc.Head.Length
                    // ...there was no hardcoded piece added on that index,
                    // just return acc and end the recursive process
                    then acc
                    // ...else continue the recursive process
                    else aux (index + 1) newAcc list
            | x::xs ->
                // Check if any hardcoded piece match current index
                let newAcc = checkCoordinates acc index

                // If lenghts are the same...
                if newAcc.Head.Length = acc.Head.Length
                    // ...there was no hardcoded piece added on that index,
                    // append normal piece instead and proceed...
                    then
                        let acc2 = checkAdjecentWords index acc x
                        let acc2Head = List.tryHead acc2
                        let acc3 = match acc2Head with
                            | Some head -> ((head @ [x])::acc2.Tail)
                            | None -> [x]::acc2

                        aux (index + 1) acc3 xs
                // ...else pass the newAcc and old list instead, as the
                // hardcoded piece have a place on the current index
                else aux (index + 1) newAcc list

        // Begin recursive progress on index 0 and with an
        // empty accumulator
        aux 0 [[]] pieces

    // Helper that help find different combinations of valid words based on a
    // list of chars
    let collectWords    lettersList
                        (hardcodedLettersList: ((char * int) * int) list)
                        (adjecentWords: (int * ((char * int) list * (char * int) list)) list)
                        isWordValid
                        minLength
                        maxLength =

        // First recursive helper, with first parameter being the remaining letters
        // to be searched, the second parameter being the current valid words found
        // and the accumulator being the "word" (list of pieces) that is currently being processed
        // to find new possible words
        let rec aux (letters: (char * int) list list) (acc: (char * int) list) (currentValidWords: (char * int) list list list)  =
            // If the word exceeds the maximum index (which means it will go over the
            // board limits), then it will just return the current valid words, else
            // it will continue its recursive run
            if maxLength <= (acc.Length + hardcodedLettersList.Length) || not State.isRunningAsync
                then
                    currentValidWords
                else
                    // Gets a list of char lists of different first letter combinations
                    let letterStarts = getAllStarts letters

                    // Internal helper function to determine if the same first letter exists
                    // within one of the other lists, if it does it returns true
                    let contains (list: (char * int) list list list) (elm: (char * int) list list) =
                        List.exists (fun (l: (char * int) list list) -> obj.ReferenceEquals(l.Head, elm.Head)) list

                    // Filter the lists out that contains the same first letter
                    let filteredList =
                        List.fold (fun (acc: (char * int) list list list) (list: (char * int) list list) ->
                                    if contains acc list then acc else list::acc) [] letterStarts

                    // Map over the list of char lists and search for valid words
                    let validWords = List.map (fun list -> aux2 list acc currentValidWords) filteredList


                    // Flatten the lists and return a list of all valid words
                    List.reduce ( @ ) validWords

        // Second recursive helper, which ensures that wildcards are treated properly.
        and aux2 letters (acc: (char * int) list) (currentValidWords: (char * int) list list list) =
            match letters with
            | [] -> currentValidWords
            | x::xs ->
                if not State.isRunningAsync then
                    currentValidWords
                else
                    let potentialNewWords = List.map (fun letter -> aux3 xs (acc @ [letter]) currentValidWords ) x

                    // Add the char to the word string
                    List.reduce ( @ ) potentialNewWords

        // Third and final helper that checks if the passed pieces forms a vaild
        // word, and in that case append it to the accumulator of valid words,
        // and finally continue the process with the remaining pieces of the hand
        and aux3 xs (acc: (char * int) list) (currentValidWords: (char * int) list list list) =
            if not State.isRunningAsync then
                currentValidWords
            else
                // Check if words at current step are valid
                let validWords = if hardcodedLettersList.IsEmpty
                                    then
                                        // If there is no hardcoded letters, just check if the acc word is valid
                                        if (acc.Length >= minLength) && isWordValid (convertPiecesToString acc)
                                            then [acc]::currentValidWords
                                            else currentValidWords
                                    else
                                        if (acc.Length >= minLength) then
                                            // ...else merge the current hand and hardcoded letters together, and check
                                            // if all words (including those that comes with the adjecent words) are
                                            // valid
                                            let wordsWithHardcodedLetters = mergePiecesAndHardcodedLetters acc hardcodedLettersList adjecentWords
                                            let wordsBool = List.fold (fun accx word -> isWordValid (convertPiecesToString word)::accx) [] wordsWithHardcodedLetters
                                            if wordsBool |> List.contains false
                                                then currentValidWords
                                                else wordsWithHardcodedLetters::currentValidWords
                                        else
                                            currentValidWords

                // Call first recursive function with the new word and list
                aux xs acc validWords

        // Invoke search for all valid words given list of chars
        let allValidWords = aux lettersList [] []

        // Remove redundant words and return the list
        Set.toList (Set.ofList (allValidWords))
