module App where

import Prelude

import Elmish (Transition, Dispatch, ReactElement, (<|))
import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Boot (defaultMain)
import Effect

data Message

type State = Unit

init :: Transition Message State
init = pure unit

update :: State -> Message -> Transition Message State
update _ _ = pure unit

view :: State -> Dispatch Message -> ReactElement
view _ _ =
    H.div "p-4"
        [ H.text "initial test "
        , H.strong "" "strong text"
        ]

main :: Effect Unit
main = defaultMain { def: { init, view, update}, elementId: "app"}