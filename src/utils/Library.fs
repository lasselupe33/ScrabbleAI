namespace utils

module Say =
    let hello name =
        printfn "Hello %s" name

module MultiSet =
    type MultiSet<'a when 'a : comparison> = | M of Map<'a, uint32>   
                                                override q.ToString() = let (M set) = q
                                                                        let formattedList = (Map.fold (fun acc k v -> sprintf "%s(%A, #%d), " acc k v)) "" set 
                                                                        "{" + formattedList.[..formattedList.Length-3] + "}"
    let empty : MultiSet<'a> = M Map.empty

    let isEmpty (M set) = Map.isEmpty set

    let size (M set) = Map.fold (fun acc _ v -> acc + v) 0u set

    let contains a (M set) = Map.containsKey a set

    let numItems a (M set) = if(contains a (M set)) then Map.find a set else 0u

    let add a n (M set) = M(Map.add a (numItems a (M set) + n) set)

    let addSingle a set  = add a 1u set      
            
    let remove a n (M set) = 
                let valBefore = numItems a (M set)                
                if(n >= valBefore) then M (Map.remove a set) else M(Map.add a (valBefore - n) set)        

    let removeSingle a (set : MultiSet<'a>) = remove a 1u set

    let fold f acc (M set) = Map.fold f acc set

    let foldBack f (M set) acc = Map.foldBack f set acc

    let map f set = fold (fun acc k v -> add (f k) v acc) empty set

    let ofList lst = List.fold (fun acc k -> addSingle k acc) empty lst

    let toList (M set) = fold (fun acc k v -> acc @ [for i in 1u..v do yield k]) List.Empty (M set)

    let union (M s) (M t) =
       let keys = s |> Map.toSeq |> Seq.map fst
       Seq.fold (fun acc x -> add x (s.Item x) acc) (M t) keys

    let subtract (M s) (M t) =
       let keys = t |> Map.toSeq |> Seq.map fst
       Seq.fold (fun acc x -> remove x (t.Item x) acc) (M s) keys

    let intersection (M a) (M b) =
       let rest = subtract (M a) (M b)
       subtract (M a) rest

module Dictionary =
    type Dictionary =
        | Leaf of bool
        | Node of (bool * Dictionary[])

    let mutable acceptedChars: char[] = Array.empty

    let empty (s: string) = acceptedChars <- s.ToCharArray(); Leaf false

    let generateEmptyNodeArr l = Array.create l (Leaf false)

    let getLetterIndex char = Array.findIndex (fun acceptedChar -> char = acceptedChar) acceptedChars

    let rec insert (x: string) (dict: Dictionary) =
        match dict with
        | Leaf _ when x.Length = 0                -> Leaf true
        | Node (_, subDicts) when x.Length = 0    -> Node(true, subDicts)

        | Leaf b ->
            let arr = generateEmptyNodeArr acceptedChars.Length
            arr.[getLetterIndex (x.Chars 0)] <- insert x.[1..] (Leaf false)
            Node(b, arr)

        | Node (b, dictArr) ->
            dictArr.[getLetterIndex (x.Chars 0)] <- insert x.[1..] dictArr.[getLetterIndex (x.Chars 0)]
            Node(b, dictArr)

    let rec lookup (x: string) (dict: Dictionary) =
        match dict with
        | Leaf b when x.Length = 0 -> b
        | Leaf _                   -> false

        | Node(b, _) when x.Length = 0 -> b
        | Node(_, dictArr) ->
            let dictToSearch = dictArr.[getLetterIndex (x.Chars 0)]
            lookup x.[1..] dictToSearch

