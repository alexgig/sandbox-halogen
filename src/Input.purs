module Input
    ( Slot
    , Query
    , component
    )
    where
      

import Prelude

import Affjax as AX
import Affjax.ResponseFormat as AXRF
import Data.Argonaut.Core as J
import Data.Argonaut.Parser as J
import Data.Either (hush)
import Data.Maybe (Maybe(..))
import Effect.Aff.Class (class MonadAff)
import Foreign.Object (Object)
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
    , response :: Maybe (Object J.Json)
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
        , HH.div_ [ HH.text $ show $ (J.fromObject >>> J.stringify) <$> state.response ]
        ]


handleAction :: forall output message. MonadAff message => Action -> H.HalogenM State Action () output message Unit
handleAction action =
    case action of
        ChangedUsername value ->
            H.modify_ \state -> state { username = value }
        FetchUser -> do
        --TODO What tells us that we are operating on the Halogen Monad in this do block?
            username <- H.gets _.username
            H.modify_ (_ {loading = true})
            response <- H.liftAff $ AX.get AXRF.json ("https://api.github.com/users/" <> username)
            --TODO how do I access the response body properties? (i.e. show body.name)
            H.modify_ (_ {loading = false, response = J.toObject =<< _.body <$> (hush response) })