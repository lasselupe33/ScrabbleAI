module EvaluateScore

    type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>

    let emptyTile = (' ', Map.empty)
    let addFunc (t : tile) p f : tile = 
        let (c, m) = t
        (c, 
         match Map.tryFind p m with
         | Some f' -> Map.add p (fun i cs -> f i cs >> f' i cs) m
         | None    -> Map.add p f m)

    let setChar c (_, m) : tile = (c, m)

    let singleLetterScore : tile = addFunc emptyTile 0u (fun i cs p -> p + (cs.[int i] |> snd))
    let doubleLetterScore : tile = addFunc emptyTile 0u (fun i cs p -> p + (cs.[int i] |> snd) * 2) |> setChar 'a'
    let tripleLetterScore : tile = addFunc emptyTile 0u (fun i cs p -> p + (cs.[int i] |> snd) * 3) |> setChar 'b'

    let doubleWordScore    = addFunc singleLetterScore 1u (fun _ _ p -> p * 2) |> setChar 'c'
    let tripleWordScore    = addFunc singleLetterScore 1u (fun _ _ p -> p * 3) |> setChar 'd'

    let calculatePoints (tiles: tile list) (pieces: (char * int) []): int =
        //let getHighestPriority (tiles: tile list) = List.reduce (fun acc elm -> (snd elm). ) tiles

        let tileFuncList (tiles: tile list) = 
            List.map (fun tile -> Map.toList (snd tile)) tiles |>
            List.reduce (fun acc sublist -> acc @ sublist)



        let test = tileFuncList tiles
        6
        //let accumulateRun (currPriority: int) (tiles: tile list) (pieces: (char * int) []) = Map.to
        
        //accumulateRun 6 tiles pieces

