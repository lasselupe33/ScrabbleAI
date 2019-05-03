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
    let getBestResult res = List.sortBy (fun r -> (fst r) * -1) res |> List.tryHead

    // Entry point for the AI which ensres to call all other methods in an async
    // fashion in order to determien the best playable word based on the current
    // state
    let getBestWord (moves: Map<ScrabbleUtil.coord, char * int>) (board: board) (hand: MultiSet<uint32>) isValidWord pieces (timeout: uint32 option) turn =
        WordPlacer.cache <- Map.empty
        State.isRunningAsync <- true
        let parsedHand = convertHandToPieceList hand pieces

        if moves.IsEmpty then
            getBestResult (evaluateValidWords (getAllWordPositions moves board board.center parsedHand isValidWord) board moves)
        else
            let mutable result = List.empty
            let mutable iterations = 0
            let movesList = if turn % 2 = 0 then Map.toList moves else List.rev (Map.toList moves)
            let timer = System.Diagnostics.Stopwatch.StartNew()

            // Duplicated code due to CancellationToken not working in other cases
            // for some reason
            match timeout with
                | Some tout ->
                    let po = new ParallelOptions()
                    po.MaxDegreeOfParallelism <- System.Environment.ProcessorCount
                    use cts = new System.Threading.CancellationTokenSource(int tout - 2500) // give time for tasks to finish up etc
                    po.CancellationToken <- cts.Token

                    try
                        Parallel.ForEach (movesList, po, (fun source state i ->
                            if state.ShouldExitCurrentIteration then
                                State.isRunningAsync <- false
                                result <- result
                            else
                                let newList = (evaluateValidWords (getAllWordPositions moves board (fst source) parsedHand isValidWord) board moves)::result
                                iterations <- iterations + 1
                                if iterations % 10 = 0 then
                                    printfn "Completed %A iterations, has %A results so far" iterations (List.fold (fun acc (resultSet: (int * (coord * (char * int)) list) list) -> acc + resultSet.Length) 0 result)
                                result <- newList)) |> ignore
                    with
                    | :? System.OperationCanceledException ->  printfn "Timed out after %A" timer.Elapsed

                    printfn "Results: %A" (List.fold (fun acc (resultSet: (int * (coord * (char * int)) list) list) -> acc + resultSet.Length) 0 result)
                    let best = getBestResult (flatten result)
                    printfn "Processing took: %A\n" timer.Elapsed
                    timer.Stop()
                    best
                | None ->
                    let po = new ParallelOptions()
                    po.MaxDegreeOfParallelism <- System.Environment.ProcessorCount
                    use cts = new System.Threading.CancellationTokenSource(15000) // Ensure we never go over 15s, even in cases of wildcards (ain't nobody got time for that)
                    po.CancellationToken <- cts.Token

                    try
                        Parallel.ForEach (movesList, po, (fun source state i ->
                            if state.ShouldExitCurrentIteration then
                                State.isRunningAsync <- false
                                result <- result
                            else
                                let newList = (evaluateValidWords (getAllWordPositions moves board (fst source) parsedHand isValidWord) board moves)::result
                                iterations <- iterations + 1
                                if iterations % 10 = 0 then
                                    printfn "Completed %A iterations, has %A results so far" iterations (List.fold (fun acc (resultSet: (int * (coord * (char * int)) list) list) -> acc + resultSet.Length) 0 result)
                                result <- newList)) |> ignore
                    with
                    | :? System.OperationCanceledException ->  printfn "Timed out after %A" timer.Elapsed

                    printfn "Results: %A" (List.fold (fun acc (resultSet: (int * (coord * (char * int)) list) list) -> acc + resultSet.Length) 0 result)
                    let best = getBestResult (flatten result)
                    printfn "Processing took: %A\n" timer.Elapsed
                    timer.Stop()
                    best


