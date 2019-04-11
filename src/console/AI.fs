namespace console
open System.IO
open Utils

module AI =

    //let switch elm (list: char list) = seq { for i in 1 .. list.Length do list @ [elm] }
    
    let collectWords lettersList =
        let rec aux (letters: char list) (word: string) (acc: string list) =
            match letters with
            | [] -> acc
            | x::xs -> 
                let newWord = word + string x
                let validWords = if isWordValid (newWord) then newWord::acc else acc
                
                aux xs newWord validWords
                
        aux lettersList "" []
