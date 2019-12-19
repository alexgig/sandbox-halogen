module Input
    ( Slot
    , Query
    , component
    )
    where
      

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Maybe (Maybe(..))
import Data.Either (hush)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP


data Query a
    = Query


type Slot = H.Slot Query Unit


type State =
    { username :: String
    , loading :: Boolean
    , response :: Maybe (AX.Response String)
    }


data Action
    = ChangedUsername String
    | FetchUser


component :: forall input output message. MonadAff message => H.Component HH.HTML Query input output message
component =
    H.mkComponent
        { initialState
        , render
        , eval: H.mkEval $ H.defaultEval { handleAction = handleAction }
        }


initialState :: forall input. input -> State
initialState input =
    { username: ""
    , loading: false
    , response: Nothing
    }


render :: forall message. State -> H.ComponentHTML Action () message
render state =
    HH.div []
        [ HH.input
            [ HP.value state.username
            , HE.onValueInput (Just <<< ChangedUsername)
            ]
        , HH.button
            [ HE.onClick $ const $ Just FetchUser 
            ]
            [ HH.text "Fetch"
            ]
        , HH.text $ show state.loading
        , HH.div_ [ HH.text $ show state.response ]
        ]


handleAction :: forall output message. MonadAff message => Action -> H.HalogenM State Action () output message Unit
handleAction action =
    case action of
        ChangedUsername value ->
            H.modify_ \state -> state { username = value }
        FetchUser -> do
            username <- H.gets _.username
            H.modify_ (_ {loading = true})
            response <- H.liftAff $ AX.get AXRF.string ("https://api.github.com/users/" <> username)
            H.modify_ (_ {loading = false, response = hush response })