module Input
    ( Slot
    , Query
    , component
    )
    where
      

import Prelude
import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data Query a
    = Query


type Slot = H.Slot Query Unit


type State =
    { username :: String
    }


data Action
    = ChangedUsername String


component :: forall input output message. H.Component HH.HTML Query input output message
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }


initialState :: forall input. input -> State
initialState input =
    { username: ""
    }


render :: forall message. State -> H.ComponentHTML Action () message
render state =
    HH.div []
        [ HH.input
            [ HP.value state.username
            , HE.onValueInput (Just <<< ChangedUsername)
            ]
        ]


handleAction :: forall output message. Action -> H.HalogenM State Action () output message Unit
handleAction action =
    case action of
        ChangedUsername value ->
            H.modify_ \state -> state { username = value }