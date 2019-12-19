module Counter
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


data Query a
    = Query


type Slot = H.Slot Query Unit


type State =
    { count :: Int 
    }


data Action
    = Increment
    | Decrement


component :: forall input output message. H.Component HH.HTML Query input output message
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }


initialState :: forall input. input -> State
initialState input =
    { count: 0
    }


render :: forall message. State -> H.ComponentHTML Action () message
render state =
    HH.div []
        [ HH.button [ HE.onClick \_ -> Just Increment ] [ HH.text "Increment" ]
        , HH.button [ HE.onClick \_ -> Just Decrement ] [ HH.text "Decrement" ]
        , HH.span [] [ HH.text $ "Count: " <> show state.count ]
        ]


handleAction :: forall output message. Action -> H.HalogenM State Action () output message Unit
handleAction action =
    case action of
        Increment ->
            H.modify_ \state -> state { count = state.count + 1 }
        Decrement ->
            H.modify_ \state -> state { count = state.count - 1 }
