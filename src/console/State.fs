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
    }

    let mkState p lp h cp plist = { 
        ownPoints = p; 
        lettersPlaced = lp; 
        hand = h; 
        currentPlayerId = cp;
        playerList = plist;
    }

    let newState hand startPlayerId playerList = mkState 0 Map.empty hand startPlayerId playerList

    let lettersPlaced st = st.lettersPlaced
    let hand st          = st.hand
