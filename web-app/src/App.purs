module App (main) where

import Prelude

import Elmish (transition, Transition, Dispatch, ReactElement, forkVoid, (<|))
--import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Boot (defaultMain)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, Aff)
import Effect.Console (logShow)
import Data.Map as DM
import Fetch (fetch)
import Data.Either (hush)
import Data.Maybe (maybe)
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as DL
import Data.Array as DA
import Data.Argonaut as Argo
import Data.Codec.Argonaut (JsonCodec, object, record, recordProp, array, string, decode)
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
import Type.Proxy (Proxy(..))
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable

type BreedInfo =
    { name :: String
    , sub_breeds :: Array String
    , cached_images :: Array String
    , cached_image_count :: Int
    }

type BreedCache = DM.Map String BreedInfo

data Message
    = BreedListLoaded String
    | BreedListLoadFailed String
    | ViewBreedList
    | LoadBreedDetails String Int
derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
    show = genericShow

data State
    = FetchingInitialList
    | ViewingLoadFailure String
    | ViewingBreedList BreedCache
    | FetchingBreedDetails BreedCache String
    | ViewingBreedDetails BreedCache String Int
derive instance genericState :: Generic State _
instance showState :: Show State where
    show = genericShow

fetch_breeds =
    do
        let requestUrl = "http://localhost:8080/api/breeds"
        {status, text} <- fetch requestUrl {}
        as_text <- text
        pure $ BreedListLoaded as_text

init :: Transition Message State
init =
    transition FetchingInitialList [fetch_breeds]

update :: State -> Message -> Transition Message State
update FetchingInitialList (BreedListLoaded string_data) =
    do
        forkVoid $ aff_log_show string_data
        pure $ update_initial_list_fetched string_data
update _ (LoadBreedDetails breed_name page_num) =
    do
        -- get from cache
        -- on cache mis get from api and re-cache
update state msg = do
    do
        forkVoid $ aff_log_show {ignored_msg : msg, ignored_state : state}
        pure state

aff_log_show :: forall a. Show a => a -> Aff Unit
aff_log_show thing =
    let
        effect_box = logShow thing
        aff_function = \_ -> (nonCanceler <$ effect_box)
    in
        makeAff aff_function

update_initial_list_fetched :: String -> State
update_initial_list_fetched string_data =
    maybe (ViewingLoadFailure "unknonw load failure") finalize_cache_to_state do
        json <- hush $ Argo.parseJson string_data
        {message} <- hush $ decode breed_list_codec json
        pure message

finalize_cache_to_state :: DM.Map String (Array String) -> State
finalize_cache_to_state in_map =
    ViewingBreedList $ fixup_cache_map in_map

fixup_cache_map :: DM.Map String (Array String) -> BreedCache
fixup_cache_map in_map =
    mapWithIndex cache_map in_map

cache_map :: String -> Array String -> BreedInfo
cache_map key value =
    { name : key, sub_breeds : value, cached_images : ([] :: Array String), cached_image_count : 0}

breed_list_codec :: JsonCodec {message :: (DM.Map String (Array String))}
breed_list_codec =
    CAR.object "message wrapper" { message : CAC.strMap (array string) }

view :: State -> Dispatch Message -> ReactElement
view FetchingInitialList _ =
    H.div "p-4"
        [ H.text "Loading local cache..." ]
view (ViewingLoadFailure failureMessage) _ =
    H.div "p-4"
        [ H.text $ "Local cache failure: " <> failureMessage ]
view (ViewingBreedList breed_cache) dispatch =
    view_breed_list dispatch breed_cache
view _ _ =
    H.div "p-4"
        [ H.text "cache loaded."
        ]

view_breed_list dispatch breed_cache =
    foldl (breed_list_entry_fold dispatch) [] breed_cache
    # H.ul ""

breed_list_entry_fold dispatch acc breed_info =
    DA.sort breed_info.sub_breeds
    # map (breed_detail_link dispatch)
    # map DA.singleton
    # map (H.li "") 
    # H.ul ""
    # \e -> [ breed_detail_link dispatch breed_info.name, e]
    # H.li ""
    # DA.snoc acc

breed_detail_link dispatch breed_name =
    H.a_ "" {onClick:dispatch <| LoadBreedDetails breed_name 0}
        [ H.text breed_name ]


main :: Effect Unit
main = defaultMain { def: { init, view, update}, elementId: "app"}
