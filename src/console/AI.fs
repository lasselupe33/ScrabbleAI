namespace console
open System.IO
open Utils
open WordFinder
open System.Threading.Tasks
open ScrabbleUtil
open utils.MultiSet
open WordPlacer
open System
open EvaluateScore

module AI =
    let getBestResult res = List.tryHead (List.sortBy (fun word -> -(fst word)) res)

    // Entry point for the AI which ensres to call all other methods in an async
    // fashion in order to determien the best playable word based on the current
    // state
    let getBestWord (moves: Map<ScrabbleUtil.coord, char * int>) (board: board) (hand: MultiSet<uint32>) isValidWord pieces =
        WordPlacer.cache <- Map.empty
        let parsedHand = convertHandToPieceList hand pieces

        if moves.IsEmpty then
            (getAllWordPositions moves board board.center parsedHand isValidWord)
        else
            let movesList = Map.toList moves
            let maxThreads = 4.0
            let temp = float movesList.Length / maxThreads
            let amountOfBatches = int (float movesList.Length / temp)
            let batchSize = int (Math.Ceiling temp)

            let tasks = [for i in 0..(amountOfBatches) do yield async {
                let maxInBatch = min batchSize (movesList.Length - (i + 1) * batchSize)
                let subMoves = [for j in 0..(maxInBatch) do yield getAllWordPositions moves board (fst movesList.[i * batchSize + j]) parsedHand isValidWord]

                return subMoves
            }]

            let scores = Array.toList (Async.RunSynchronously (Async.Parallel tasks)) |> fun asyncResult -> flatten (flatten asyncResult)
            scores
