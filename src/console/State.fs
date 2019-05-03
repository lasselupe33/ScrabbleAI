namespace console

open utils
open ScrabbleUtil

module State =
    type state = {
        currentPlayerId : uint32
        playerList      : (uint32 * string) list
        ownPoints       : int
        lettersPlaced   : Map<ScrabbleUtil.coord, char * int>
        hand            : MultiSet.MultiSet<uint32>
        tilesLeft       : uint32
        turn            : int
    }

    let mutable isRunningAsync = false

    let mkState p lp h cp plist tleft t = {
        ownPoints = p;
        lettersPlaced = lp;
        hand = h;
        currentPlayerId = cp;
        playerList = plist;
        tilesLeft = tleft;
        turn = t;
    }

    let newState hand startPlayerId playerList tiles = mkState 0 Map.empty hand startPlayerId playerList tiles 0

    let lettersPlaced st = st.lettersPlaced
    let hand st          = st.hand
