namespace console
open System.IO

open utils

module AI =
    let readLines (filePath:string) = seq {
        use sr = new StreamReader (filePath)
        while not sr.EndOfStream do
            yield sr.ReadLine ()
    }

    let dict (words: seq<string>) = Seq.fold(fun dict word -> word) Dictionary.empty