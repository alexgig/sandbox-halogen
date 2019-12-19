module App
    ( Slot
    , Query
    , component
    )
    where


import Prelude

import Counter as Counter
import Data.Maybe (Maybe(..))
import Data.Symbol (SProxy(..))
import Halogen as H
import Halogen.HTML as HH


data Query a
    = Query


type Slot = H.Slot Query Unit


type Slots =
    ( counter :: Counter.Slot Unit
    )


_counter :: SProxy "counter"
_counter = SProxy


component :: forall input output message. H.Component HH.HTML Query input output message
component =
    H.mkComponent
        { initialState: const unit
        , render
        , eval: H.mkEval $ H.defaultEval
        }


render :: forall state action message. state -> H.ComponentHTML action Slots message
render state =
    HH.div []
        [ HH.slot _counter unit Counter.component unit (\_ -> Nothing)
        ]


