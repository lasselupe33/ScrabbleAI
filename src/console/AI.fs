namespace console
open System.IO
open Utils
open WordFinder
open System.Threading.Tasks

module AI =
    let getPieceChar (piece: char * int) = fst piece
    let convertPiecesToString (pieces: (char * int) list) = List.fold (fun (acc: string) (piece: char * int)  -> acc + string (getPieceChar piece)) "" pieces

    // Helper that help find different combinations of valid words based on a
    // list of chars
    let collectWords lettersList =

        // First recursive helper, with first parameter being the remaining letters
        // to be searched, the second parameter being the current valid words found
        // and the accumulator being the "word" (list of pieces) that is currently being processed
        // to find new possible words
        let rec aux (letters: (char * int) list list) (words: (char * int) list list) (acc: (char * int) list) =

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

            // Map over the list of char lists and search for valid words, however
            // on the first run, start the tasks on async threads in order to speed
            // up computation (went from ~14ms to ~5ms for a hand of 7 pieces)
            let validWords =
                if acc.Length = 0 then
                    let tasks = [for i in 0..(filteredList.Length - 1) do yield async { return aux2 filteredList.[i] acc words } ]
                    Array.toList (Async.RunSynchronously (Async.Parallel tasks))
                else
                    List.map (fun list -> aux2 list acc words) filteredList


            // Flatten the lists and return a list of all valid words
            List.reduce ( @ ) validWords

        // Second recursive helper, which ensures that wildcards are treated properly.
        and aux2 letters (word: (char * int) list) (acc: (char * int) list list) =
            match letters with
            | [] -> acc
            | x::xs -> 
                let potentialNewWords = List.map (fun letter -> aux3 xs (word @ [letter]) acc) x

                // Add the char to the word string
                List.reduce ( @ ) potentialNewWords

        // Third and final helper that checks if the passed pieces forms a vaild
        // word, and in that case append it to the accumulator of valid words, 
        // and finally continue the process with the remaining pieces of the hand
        and aux3 xs (newPieces: (char * int) list) (acc: (char * int) list list) =
            // If word is valid, then add it to the list of valid words
            let validWords = if isWordValid (convertPiecesToString newPieces) then newPieces::acc else acc

            // Call first recursive function with the new word and list
            aux xs validWords newPieces

        // Invoke search for all valid words given list of chars
        let allValidWords = aux lettersList [] []
        
        // Remove redundant words and return the list
        Set.toList (Set.ofList (allValidWords))
