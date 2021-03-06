module EvaluateScore
open ScrabbleUtil
open MBrace.FsPickler.CSharpProxy

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


    // Method used to calculate the amount of points a given word is worth
    let calculatePoints (tiles: tile list) (pieces: (char * int)[]): int =
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
        let prepareTiles (tiles: tile list) = 
            List.mapi (fun index tile -> 
                (fst tile, List.map 
                    (fun tileFunc -> 
                        (fst tileFunc, insertWordIntoTileFunction (uint32 index) pieces (snd tileFunc))) (getTileFunctions tile))) tiles

        // Helper that flatten the tiles into a list containing the functions.
        // It extract the second element of the tuple in the tile, and
        // extract the functions out of the maps into a list
        let flatten tiles =
            List.map (fun tile -> (snd tile)) tiles |>
            List.reduce ( @ )

        // Helper that compares the first element of two tuples
        let tileFuncComparator t1 t2 =
            if fst t1 > fst t2 then 1 else -1

        // Helper that sorts a list by comparing the first element of a tuple
        let sortFuncTilesByPriority lst = List.sortWith tileFuncComparator lst

        // Helper that maps over a list of tuples and returns a new list
        // containing the second element
        let extract = List.map snd

        // Helper that counts the points by folding over the list of functions,
        // invoking them and add the results to an accumilator
        let composer = List.fold (fun acc tileFunc -> tileFunc acc) 0

        // Calculate points and returns it
        composer (extract (sortFuncTilesByPriority (flatten (prepareTiles tiles))))


    let evaluateValidWords (collectionOfWordClauses: (coord * (char * int)) list list list) (board: board) (lettersPlaced: Map<coord,(char * int)>) =
        let getTile coord = Option.get (board.tiles (coord))

        let prepareTiles (piecesWithCoord: (coord * (char * int)) list) isMainWord =
            List.fold (fun acc letter ->
                if Option.isSome(lettersPlaced.TryFind(fst letter)) || not isMainWord
                    then acc @ [board.usedTile]
                    else acc @ [(getTile (fst letter))]) [] piecesWithCoord

        let prepareWords (piecesWithCoord: (coord * (char * int)) list) =
            List.fold (fun acc letter -> acc @ [(snd letter)]) [] piecesWithCoord |>
            List.toArray

        let prepareTilesAndWords (wordsWithCoord: (coord * (char * int)) list list) = 
            List.fold (fun acc words -> acc @ [((prepareTiles words acc.IsEmpty, prepareWords words), words)]) [] wordsWithCoord

        let tilesAndWords = List.fold (fun acc wordClause -> (prepareTilesAndWords wordClause)::acc) [] collectionOfWordClauses

        let calculate tileWord = calculatePoints (fst tileWord) (snd tileWord)

        let evaluate = List.fold (fun acc word -> (calculate (fst word)) + acc) 0

        let evaluateClause wordClause = 
            List.fold (fun acc word -> ((evaluate word), snd word.Head)::acc) [] wordClause
            
        let result = tilesAndWords |> evaluateClause

        result
