namespace console

open utils

module Print =

    let printHand pieces (hand: MultiSet.MultiSet<uint32>) =
        hand |>
        MultiSet.fold (fun _ x i -> printfn "%d -> (%A, %d)" x (Map.find x pieces) i) ()
