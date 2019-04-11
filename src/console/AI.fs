namespace console
open System.IO
open Utils
open WordFinder

module AI =

    // Gamle metode, bare så du kan appreciate opdateringen, Lasse ;)
    let collectWordsOld lettersList =
        let rec aux (letters: char list) (word: string) (acc: string list) =
            match letters with
            | [] -> acc
            | x::xs -> 
                let newWord = word + string x
                let validWords = if isWordValid (newWord) then newWord::acc else acc
                
                let letterStarts = getAllStarts xs
                let validWords2 = List.map (fun list -> aux list newWord validWords) letterStarts

                List.reduce ( @ ) validWords2
        
        let letterStarts = getAllStarts lettersList
        let allValidWords = List.map (fun list -> aux list "" []) letterStarts

        Set.toList (Set.ofList (List.reduce ( @ ) allValidWords))    
        



    // Helper that help find different combinations of valid words based on a 
    // list of chars    
    let collectWords lettersList =

        // First recursive helper 
        let rec aux letters word acc =

            // Gets a list of char lists of different combinations of each letter
            // starting at index zero
            let letterStarts = getAllStarts letters

            // Map over the list of char lists and search for valid words
            let validWords2 = List.map (fun list -> aux2 list acc word) letterStarts

            // Flatten the lists and return a list of all valid words
            List.reduce ( @ ) validWords2

        // Second recursive helper
        and aux2 letters word acc =
            match letters with
            | [] -> acc
            | x::xs -> 
                // Add the char to the word string
                let newWord = word + string x

                // If word is valid, then add it to the list of valid words
                let validWords = if isWordValid (newWord) then newWord::acc else acc

                // Call first recursive function with the new word and list
                aux xs validWords newWord
        
        // Invoke search for all valid words given list of chars
        let allValidWords = aux lettersList [] ""
        
        // Remove redundant words and return the list
        Set.toList (Set.ofList (allValidWords))   