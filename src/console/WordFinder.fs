namespace console

open utils
open utils.MultiSet
open Utils

module WordFinder =
    // Simple helper that based on a pieceId extracts the Char that is related to
    // it.
    // NOTE: It doesn't handle wildcards gracefully yet!
    let getPiece pieceId = (Set.toList (Map.find pieceId State.pieces))

    // Helper that converts the hand of piece identifiers to a list of characters
    let convertHandToPieceList (hand: MultiSet<uint32>) = List.map getPiece (MultiSet.toList hand)

    // Helper that converts all takes a list and returns all possible permutations
    // of the same length
    let rec permutations (input: 'a list) = seq {
        if (input.IsEmpty) then 
            yield []
        else
            
        for i in input do
        yield! input
                |> List.filter (fun x-> x<>i) 
                |> permutations
                |> Seq.map (fun x->i::x)
        }

    let getAllStarts (list: (char * int) list list) =
        let rec aux n result =     
            let length = List.length list
            if (n > 0 && n < length) then
                aux (n - 1) ((List.permute (fun index -> (index + n) % length) list)::result)
            else
                list::result

        aux (list.Length - 1) List.empty

    // Helper that creates all possible combinations of a list in a given length n
    let rec comb n l = 
        match n, l with
        | 0, _ -> [[]]
        | _, [] -> []
        | k, (x::xs) -> List.map ((@) [x]) (comb (k-1) xs) @ comb k xs


    // Helper that returns ALL the possible unique potential words from a list of 
    // characters
    let getAllPossibilities (lst: char list) =
        let rec aux n result = if n > 0 then aux (n - 1) (comb n lst @ result) else result

        Set.toList (Set.ofList ((aux (lst.Length - 1) List.Empty) @ (Seq.toList (permutations lst))))

    // Helper that converts a list of characters to a string
    let convertCharListToString lst = List.fold (fun word char -> word + (string char)) "" lst

    // Helper that, based on a list of potential words as char lists, returns a
    // list of words that actually exists in the dictionary
    let getValidWords (lst: char list) = 
        getAllPossibilities lst |>
        List.map convertCharListToString |>
        List.filter isWordValid