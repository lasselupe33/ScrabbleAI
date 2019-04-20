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

            // Gets a list of char lists of different combinations of each letter
            // starting at index zero
            let letterStarts = getAllStarts letters

            // Map over the list of char lists and search for valid words, however
            // on the first run, start the tasks on async threads in order to speed
            // up computation (went from ~14ms to ~5ms for a hand of 7 pieces)
            let validWords2 = 
                if acc.Length = 0 then 
                    let tasks = [for i in 0..(letterStarts.Length - 1) do yield async { return aux2 letterStarts.[i] acc words } ]
                    Array.toList (Async.RunSynchronously (Async.Parallel tasks))
                else
                    List.map (fun list -> aux2 list acc words) letterStarts


            // Flatten the lists and return a list of all valid words
            List.reduce ( @ ) validWords2

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