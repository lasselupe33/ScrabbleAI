namespace ScrabbleUtil

type coord = int * int

module Coord =

    let mkCoordinate x y = (x, y)
    let getX (x, _) = x
    let getY (_, y) = y

type tile = char * Map<uint32, uint32 -> (char * int)[] -> int -> int>

module Tile =
    let emptyTile = (' ', Map.empty)
    let addFunc (t : tile) p f : tile = 
        let (c, m) = t
        (c, 
         match Map.tryFind p m with
         | Some f' -> Map.add p (fun i cs -> f i cs >> f' i cs) m
         | None    -> Map.add p f m)
    let removeFunc t p = Map.remove p t

    let setChar c (_, m) : tile = (c, m)

type board = 
    { center : coord; 
      usedTile : tile;
      tiles : coord -> tile option }

module Board =
    let mkBoard c dt f = {center = c; usedTile = dt; tiles = f}
    let center b = b.center
    let usedTile b = b.usedTile
    let tiles b = b.tiles

type piece = Set<char * int>

module Piece =

    let singleLetter l p : piece = Set.singleton (l, p)
    let wildCard ls : piece = ls |> Set.ofList |> Set.map (fun l -> (l, 0))

    let toString (p : piece) = 
        sprintf "{%s}" (Set.fold (fun acc (c, x) -> sprintf "%s (%c, %d) " acc c x) "" p)