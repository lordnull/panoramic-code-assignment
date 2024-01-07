module App (main) where

import Prelude

import Elmish (transition, Transition, Dispatch, ReactElement, fork, forkVoid, (<|))
--import Elmish.HTML.Events as E
import Elmish.HTML.Styled as H
import Elmish.Boot (defaultMain)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, Aff)
import Effect.Console (logShow)
import Data.Map as DM
import Fetch (fetch)
import Data.Either (Either(..), hush, note, either)
import Data.Maybe (maybe, Maybe(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.List as DL
import Data.Array as DA
import Data.Int (toNumber, ceil)
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
    | ViewBreedDetails String Int
    | BreedDetailsLoaded String String
    | BreedDetailsFailed String String
derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
    show = genericShow

data State
    = FetchingInitialList
    | ViewingLoadFailure String
    | ViewingBreedList (Maybe String) BreedCache
    | FetchingBreedDetails BreedCache String Int
    | ViewingBreedDetails BreedCache BreedInfo Int
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
update FetchingInitialList (BreedListLoadFailed why) =
    transition (ViewingLoadFailure why) []
update FetchingInitialList ignored_msg =
    do
        forkVoid $ aff_log_show {message: "ignoring message while waiting for initial load", ignored_msg}
        pure FetchingInitialList
update state_with_cache (BreedDetailsFailed breed_name why) =
    "Could not get details for breed \"" <> breed_name <> "\" due to " <> why
    # Just
    # \err_msg -> ViewingBreedList err_msg (extract_cache state_with_cache)
    # \new_state -> transition new_state []
update (FetchingBreedDetails cache breed_name page_num) (BreedDetailsLoaded fetch_breed raw_list) | breed_name == fetch_breed =
    case parse_breed_details raw_list of
        Left _ ->
            transition (ViewingBreedList (Just (fetch_breed <> " failed to parse details.")) cache ) []
        Right image_urls ->
            let
                {breed_info, new_cache} = update_cache fetch_breed image_urls cache
            in
                transition (ViewingBreedDetails new_cache breed_info page_num) []
update state_with_cache (BreedDetailsLoaded fetch_breed raw_list) =
    case parse_breed_details raw_list of
        Left _ ->
            transition state_with_cache []
        Right image_urls ->
            let
                old_cache = extract_cache state_with_cache
                {new_cache} = update_cache fetch_breed image_urls old_cache
                new_state = insert_cache state_with_cache new_cache
            in
                transition new_state []
update state_with_cache (ViewBreedDetails breed_name page_num) =
    update_load_breed_details breed_name page_num $ extract_cache state_with_cache
update state_with_cache ViewBreedList =
    transition (ViewingBreedList Nothing (extract_cache state_with_cache)) []
update state msg = do
    do
        forkVoid $ aff_log_show {message: "Handling this state + message combo is not yet implemented", ignored_msg : msg, ignored_state : state}
        pure state

update_cache :: String -> Array String -> DM.Map String BreedInfo -> {breed_info :: BreedInfo, new_cache :: DM.Map String BreedInfo}
update_cache breed_name image_urls old_cache =
    {- I'd like to use Data.Map.insertWith, but I need to know the actual value I'm inserting, so I kinda need to
       write it myself. -}
    let
        default_info = {name:breed_name, sub_breeds:[], cached_image_count: DA.length image_urls, cached_images:image_urls}
        maybe_replaced = do
            found <- DM.lookup breed_name old_cache
            merge_fun <- pure (\existing {cached_image_count, cached_images} ->
                existing {cached_image_count = cached_image_count, cached_images = cached_images})
            pure $ merge_fun found default_info
    in
        maybe default_info identity maybe_replaced
        # \breed_info ->
            {breed_info, new_cache : DM.insert breed_name breed_info old_cache}


parse_breed_details :: String -> Either String (Array String)
parse_breed_details text =
    do
      json <- note "json parse failed" $ hush $ Argo.parseJson text
      {message} <- note "json transform failed" $ hush $ decode ( CAR.object "message wrapper" {message : array string} ) json
      pure message


extract_cache :: State -> BreedCache
extract_cache FetchingInitialList =
    -- This shouldn't happen, but to make the function complete
    DM.empty
extract_cache (ViewingLoadFailure _) =
    DM.empty
extract_cache (ViewingBreedList _ cache) =
    cache
extract_cache (FetchingBreedDetails cache _ _) =
    cache
extract_cache (ViewingBreedDetails cache _ _) =
    cache

insert_cache ::State -> BreedCache -> State
insert_cache FetchingInitialList _ =
    FetchingInitialList
insert_cache state@(ViewingLoadFailure _) _ =
    state
insert_cache (ViewingBreedList s _) cache =
    ViewingBreedList s cache
insert_cache (FetchingBreedDetails _ a b) cache =
    FetchingBreedDetails cache a b
insert_cache (ViewingBreedDetails _ a b) cache =
    ViewingBreedDetails cache a b


aff_log_show :: forall a. Show a => a -> Aff Unit
aff_log_show thing =
    let
        effect_box = logShow thing
        aff_function = \_ -> (nonCanceler <$ effect_box)
    in
        makeAff aff_function

update_load_breed_details :: String -> Int -> BreedCache -> Transition Message State
update_load_breed_details name page cache =
    DM.lookup name cache
    # note name
    # either (update_missing_entry cache) (update_load_breed_details_cont page cache)

update_missing_entry cache name =
    transition (ViewingBreedList (Just ("Could not find breed \"" <> name <> "\".")) cache) []


update_load_breed_details_cont page cache breed_info =
    case breed_info.cached_images of
        [] ->
            do
                fork do
                    {status, text} <- fetch ("http://localhost:8080/api/breed/" <> breed_info.name <> "/images") {}
                    as_text <- text
                    pure $ case status of
                        200 ->
                            BreedDetailsLoaded breed_info.name as_text
                        _not_200 ->
                            BreedDetailsFailed breed_info.name as_text
                pure $ FetchingBreedDetails cache breed_info.name page
        image_url_list ->
            transition ( ViewingBreedDetails cache breed_info page ) []







update_initial_list_fetched :: String -> State
update_initial_list_fetched string_data =
    maybe (ViewingLoadFailure "unknown load failure") finalize_cache_to_state do
        json <- hush $ Argo.parseJson string_data
        {message} <- hush $ decode breed_list_codec json
        pure message

finalize_cache_to_state :: DM.Map String (Array String) -> State
finalize_cache_to_state in_map =
    ViewingBreedList Nothing $ fixup_cache_map in_map

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
view (ViewingBreedList Nothing breed_cache) dispatch =
    view_breed_list dispatch breed_cache
view (ViewingBreedList (Just error) breed_cache) dispatch =
    H.div ""
        [ H.div "error"
            [ H.text error ]
        , view_breed_list dispatch breed_cache
        ]
view (ViewingBreedDetails cache breed_info page_num) dispatch =
    H.div "p-4"
        [ H.a_ "" {onClick : dispatch <| ViewBreedList} [ H.text "< back to list" ]
        , H.h1 "" [ H.text breed_info.name ]
        , H.h2 "" [ H.text "sub breeds"]
        , H.span "" $ map H.text $ DA.intersperse ", " breed_info.sub_breeds
        , view_page_navigator breed_info page_num dispatch
        , H.span "" $ view_images breed_info page_num
        , view_page_navigator breed_info page_num dispatch
        ]
view state _ =
    H.div "p-4"
        [ H.text "unfinished view for given state:"
        , H.text $ show state
        ]

page_size = 20

view_images {cached_images} page_num =
    let
        slice_start = (page_num - 1) * page_size
        slice_end = slice_start + page_size
    in
        DA.slice slice_start slice_end cached_images
        # map {src : _}
        # map (H.img_ "")

view_page_navigator :: BreedInfo -> Int -> (Dispatch Message) -> ReactElement
view_page_navigator breed_info page_num dispatch =
    let
        page_count = total_pages breed_info.cached_image_count page_size
    in
        H.div "p-4"
            [ H.button_ "" {disabled:page_num <= 1, onClick:dispatch <| ViewBreedDetails breed_info.name 1} [ H.text "<<"]
            , H.button_ "" {disabled:page_num <= 1, onClick:dispatch <| ViewBreedDetails breed_info.name (page_num - 1)} [ H.text "<"]
            , H.span ""
                [ H.text $ show page_num
                , H.text " / "
                , H.text $ show $ total_pages breed_info.cached_image_count page_size
                ]
            , H.button_ "" {disabled: page_num >= page_count, onClick:dispatch <| ViewBreedDetails breed_info.name (page_num + 1)} [H.text ">"]
            , H.button_ "" {disabled: page_num >= page_count, onClick:dispatch <| ViewBreedDetails breed_info.name $ page_count} [ H.text ">>"]
            ]


total_pages :: Int -> Int -> Int
total_pages item_count items_per_page =
    ceil $ (toNumber item_count) / (toNumber items_per_page)




view_breed_list dispatch breed_cache =
    foldl (breed_list_entry_fold dispatch) [] breed_cache
    # H.dl ""

breed_list_entry_fold dispatch acc breed_info =
    let
        sub_breed = sub_breed_element breed_info
        breed = breed_element dispatch breed_info
    in
        DA.snoc (DA.snoc acc breed) sub_breed

sub_breed_element {sub_breeds} =
    DA.intersperse ", " sub_breeds
    # map H.text
    # H.dd ""

breed_element dispatch {name} =
    H.text name
    # H.a_ "" {onClick:dispatch <| ViewBreedDetails name 1}
    # H.dt ""


main :: Effect Unit
main = defaultMain { def: { init, view, update}, elementId: "app"}
