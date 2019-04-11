namespace console

open utils
open ScrabbleUtil


module State = 
    type state = {
        ownPoints     : int
        lettersPlaced : Map<ScrabbleUtil.coord, char * int>
        hand          : MultiSet.MultiSet<uint32>
    }

    let mkState p lp h = { ownPoints = p; lettersPlaced = lp; hand = h }

    let newState hand = mkState 0 Map.empty hand

    let lettersPlaced st = st.lettersPlaced
    let hand st          = st.hand
