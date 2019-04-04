open utils.Say
open System

[<EntryPoint>]
let main argv =
    utils.Say.hello argv.[0]
    let dict = utils.Dictionary.empty
    let set = utils.MultiSet.empty

    0;; // return an integer exit code



