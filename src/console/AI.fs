namespace console
open System.IO
open Utils
open WordFinder
open System.Threading.Tasks

module AI =


    // Helper that help find different combinations of valid words based on a
    // list of chars
    let collectWords lettersList =

        // First recursive helper
        let rec aux letters (word: string list) acc =

            // Gets a list of char lists of different first letter combinations
            let letterStarts = getAllStarts letters

            // Internal helper function to determine if the same first letter exists
            // within one of the other lists, if it does it returns true
            let contains (list: char list list) (elm: char list) =
                List.exists (fun (l: char list) -> l.Head = elm.Head) list

            // Filter the lists out that contains the same first letter
            let filteredList =
                List.fold (fun (acc: char list list) (list: char list) ->
                            if contains acc list then acc else list::acc) [] letterStarts

            // Map over the list of char lists and search for valid words, however
            // on the first run, start the tasks on async threads in order to speed
            // up computation (went from ~14ms to ~5ms for a hand of 7 pieces)
            let validWords =
                if acc = "" then
                    let tasks = [for i in 0..(filteredList.Length - 1) do yield async { return aux2 filteredList.[i] acc word } ]
                    Array.toList (Async.RunSynchronously (Async.Parallel tasks))
                else
                    List.map (fun list -> aux2 list acc word) filteredList


            // Flatten the lists and return a list of all valid words
            List.reduce ( @ ) validWords

        // Second recursive helper
        and aux2 letters word acc =
            match letters with
            | [] -> acc
            | x::xs ->
                // Add the char to the word string
                let newWord = word + string x

                // If word is valid, then add it to the list of valid words
                let validWords2 = if isWordValid (newWord) then newWord::acc else acc

                // Call first recursive function with the new word and list
                aux xs validWords2 newWord

        // Invoke search for all valid words given list of chars
        let allValidWords = aux lettersList [] ""

        // Remove redundant words and return the list
        Set.toList (Set.ofList (allValidWords))
