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
        // Helper used to retrieve the index of a given tile
        let getIndex (tile: tile) = List.findIndex(fun x -> obj.ReferenceEquals(x, tile)) tiles

        // Helper that strips out the char used for pretty printing and then converts
        // the map of tile functions to a list
        let getTileFunctions (tile: tile) = Map.toList (snd tile)

        // Helper that takes a character index, a word (char * int []) and a tile function and inserts the given
        // word and index into the function in preparation for simple calculation of scores later on
        let insertWordIntoTileFunction index word func = func index word

        // Helper that prepares all by converting the tile functions map to a list
        // and inserting the proper character index for the appropriate letter and
        // inserts the whole word into the function as well
        let prepareTiles (tiles: tile list) = List.map (fun tile -> (fst tile, List.map (fun tileFunc -> (fst tileFunc, insertWordIntoTileFunction (uint32 (getIndex tile)) pieces (snd tileFunc))) (getTileFunctions tile))) tiles

        let flatten tiles = 
            List.map (fun tile -> (snd tile)) tiles |>
            List.reduce (fun acc sublist -> acc @ sublist)

        let tileFuncComparator e1 e2 =
            if fst e1 > fst e2 then 1 else -1

        let sortFuncTilesByPriority lst = List.sortWith tileFuncComparator lst
        
        let extract = List.map snd

        let composer = List.fold (fun acc tileFunc -> tileFunc acc) 0


        composer (extract (sortFuncTilesByPriority (flatten (prepareTiles tiles))))
