namespace ScrabbleUtil

open ScrabbleUtil.ServerCommunication
open ScrabbleUtil

module StandardBoard =
    open Tile

    let singleLetterScore : tile = addFunc emptyTile 0u (fun i cs p -> p + (cs.[int i] |> snd))
    let doubleLetterScore : tile = addFunc emptyTile 0u (fun i cs p -> p + (cs.[int i] |> snd) * 2) |> setChar 'a'
    let tripleLetterScore : tile = addFunc emptyTile 0u (fun i cs p -> p + (cs.[int i] |> snd) * 3) |> setChar 'b'

    let doubleWordScore    = addFunc singleLetterScore 1u (fun _ _ p -> p * 2) |> setChar 'c'
    let tripleWordScore    = addFunc singleLetterScore 1u (fun _ _ p -> p * 3) |> setChar 'd'

    let mirrorX ((x, y) : coord) = (-x, y)
    let mirrorY ((x, y) : coord) = (x, -y)
    let mirrorXY = mirrorX >> mirrorY

    let appendFunc f lst = List.fold (fun acc x -> (f x)::acc) lst lst
    let mapProdFst s = List.map (fun x -> (x, s))
    let mirrorBoardQuadrant t = 
       appendFunc mirrorX >> appendFunc mirrorY >> 
       (fun ts m -> List.fold (fun acc c -> Map.add c t acc) m ts)

    let mkStandardBoard () =
        let bf = 
             ([(7, 7); (7, 0); (0, 7)] |> mirrorBoardQuadrant tripleWordScore) >>        
             ([for i in 3..6 do yield (i, i)] |> mirrorBoardQuadrant doubleWordScore) >>
             ([(3, 3); (6, 3); (3, 6)] |> mirrorBoardQuadrant tripleLetterScore) >>
             ([(4, 0); (1, 1); (5, 1); (0, 4); (7, 4); (1, 5); (4, 7)] |> mirrorBoardQuadrant doubleLetterScore) 

        let tf b =
            function
            | c when Coord.getX c >= -7 && Coord.getY c >= -7  && Coord.getX c <= 7 && Coord.getY c <= 7  ->
                Some (match Map.tryFind c b with
                      | Some t -> t
                      | None -> singleLetterScore)
            | _ -> None

        Board.mkBoard (0, 0) singleLetterScore (fun c -> tf (bf Map.empty) c)