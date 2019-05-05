namespace console

open utils
open ScrabbleUtil

module State =
    type state = {
        currentPlayerId : uint32
        playerList      : (uint32 * string) list
        ownPoints       : int
        ownId           : uint32
        lettersPlaced   : Map<ScrabbleUtil.coord, char * int>
        hand            : MultiSet.MultiSet<uint32>
        tilesLeft       : uint32
        turn            : int
    }

    let mutable isRunningAsync = false

    let mkState p lp h cp oid plist tleft t = {
        ownPoints = p;
        ownId = oid;
        lettersPlaced = lp;
        hand = h;
        currentPlayerId = cp;
        playerList = plist;
        tilesLeft = tleft;
        turn = t;
    }

    let newState hand currPlayerId ownId playerList tiles = mkState 0 Map.empty hand currPlayerId ownId playerList tiles 0

    let lettersPlaced st = st.lettersPlaced
    let hand st          = st.hand
