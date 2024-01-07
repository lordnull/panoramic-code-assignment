{- A very simple web interface alternative to the dog.ceo api.
Primarily an attempt to show how effective I can be in purescript.
-}

{- The name 'App' is used to seperate it from the server component. -}
module App (main) where

import Prelude

{- As I'm already familiar with "The Elm Architecture", and there is already
a library for it in purescript, I felt it would be a faster start than trying
to work in a realatively unfamiliar language as well as an unfamiliar architecture.
To be fair, there isn't must difference between modern js frameworks like
ract and 'TEA' anyway.
-}
import Elmish (transition, Transition, Dispatch, ReactElement, fork, forkVoid, (<|))
import Elmish.HTML.Styled as H
import Elmish.Boot (defaultMain)
import Effect (Effect)
import Effect.Aff (makeAff, nonCanceler, Aff)
import Effect.Console (logShow)
import Data.Map as DM
import Fetch (fetch)
import Data.Either (Either(..), hush, note)
import Data.Maybe (maybe, Maybe(..))
import Data.FunctorWithIndex (mapWithIndex)
import Data.Array as DA
import Data.Int (toNumber, ceil)
{- Again, taking inspiration from elm, I'm using profunctor style for json decoding.
While I am aware there are libraries that will derive json encoding and decoding
from the types I write, I would rather keep the types used internally sepearte from
the data used "on the wire". This allows each side to change independently if
needed. -}
import Data.Argonaut as Argo
import Data.Codec.Argonaut (JsonCodec, array, string, decode)
import Data.Codec.Argonaut.Common as CAC
import Data.Codec.Argonaut.Record as CAR
{- I added show support to some types so I could debug them a bit easier. -}
import Data.Generic.Rep (class Generic)
import Data.Show.Generic (genericShow)
import Data.Traversable (foldl)

{- A simple record to store the core data for a dog breed. -}
type BreedInfo =
    { name :: String
    , sub_breeds :: Array String
    {- To indicate a breed has not been fully cached, I check if the cached images
    field is empty. I felt that was going to make for easier code than wrapping it
    in a 'Maybe'. Most other times, the 'Maybe' does make more sense. -}
    , cached_images :: Array String
    , cached_image_count :: Int
    }

{- since we only want to query a given breed once, we need to build up a cache. 
Again, simplicity dictacts a basic String -> BreedInfo mapping.
-}
type BreedCache = DM.Map String BreedInfo

{- The messages that will ultimately change the state of the app.
-}
data Message
    = BreedListLoaded String
    | BreedListLoadFailed String
    | ViewBreedList
    | ViewBreedDetails String Int
    | BreedDetailsLoaded String String
    | BreedDetailsFailed String String
    | BreedDetailsFound BreedInfo
derive instance genericMessage :: Generic Message _
instance showMessage :: Show Message where
    show = genericShow

{- Part of the app state. This was introduced as I was refactoring in an attempt to
simplify the update and view functions. This holds the bare minimum needed that differs between
the app's 2 pages.

Normally this would be handled by a router, but for simplicity's sake, that has been ommited from
this task (thanks!) -}
data Page
    = BreedList
    | BreedDetail BreedInfo Int
derive instance genericPage :: Generic Page _
instance showPage :: Show Page where
    show = genericShow

{- Another building bloack of the app state.The types of work the page can do.
This is used to 'lock' the ui for when it's getting information from the server.
-}
data Fetching
    = PrimaryList
    | Detail String
instance Show Fetching where
    show PrimaryList = "Getting Full breed list..."
    show (Detail breed) = "Getting image list for " <> breed

{- And putting it altogether we have the application state.
-}
type State =
    {- cache is initially and empty map. However, we have the busy_work so 
    we won't actually need to worry about that. -}
    { cache :: BreedCache
    {- If we're busy, what we're busy on goes here. While one could extend the Fetching to have
    a 'Nothing', by using 'Maybe' we can leverage all the wonderful things it can do. -}
    , busy_work :: Maybe Fetching
    {- Very similar to the busy_work above. Rather than test for an empty string, just using a maybe. -}
    , last_error :: Maybe String
    {- Not to be confused with page number! This is the page as explained at the 'Page' type. -}
    , page :: Page
    }

{- Get's the primary breed list from the server. It can then be used to fill up the cache. -}
fetch_breeds :: Aff Message
fetch_breeds =
    do
        {- TODO make this configurable -}
        let requestUrl = "http://localhost:8080/api/breeds"
        {text} <- fetch requestUrl {}
        as_text <- text
        pure $ BreedListLoaded as_text

{- The entry point for Elmish to start the web app. -}
init :: Transition Message State
init =
    transition {cache : DM.empty :: DM.Map String BreedInfo, busy_work : Just PrimaryList, page : BreedList, last_error : Nothing } [fetch_breeds]

{- Whenever a message is generated, it and the current state get fed into here.
Several different styles of code here. While I attempted to use the do
syntax as much as possible, I attempted to weigh it's readbility against
other options such as ($) and (#). Of course, readability is subjective. -}
update :: State -> Message -> Transition Message State
update state (BreedListLoaded string_data) =
    do
        _ <- forkVoid $ aff_log_show string_data
        pure $ maybe (state { last_error = Just "failed to parse breed list"}) identity
            do
                fixed_cache <- update_initial_list_fetched string_data
                pure $ state { cache = fixed_cache, busy_work = Nothing}

update state (BreedListLoadFailed why) =
    transition (state { last_error = Just why}) []

update state (BreedDetailsFailed breed_name why) =
    "Could not get details for breed \"" <> breed_name <> "\" due to " <> why
    # \e -> state { last_error = Just e}
    # maybe_not_busy breed_name
    # \new_state -> transition new_state []

update state (BreedDetailsFound breed_info) =
    transition (maybe_update_page_info breed_info state) []

update state (BreedDetailsLoaded fetch_breed raw_list) =
    case parse_breed_details raw_list of
        Left _ ->
            fetch_breed <> " failed to parse details."
            # \e -> state { last_error = Just e}
            # maybe_not_busy fetch_breed
            # \new_state -> transition new_state []
        Right image_urls ->
            let
                {breed_info, new_cache} = update_cache fetch_breed image_urls state.cache
            in
                state { cache = new_cache}
                # maybe_update_page_info breed_info
                # maybe_not_busy fetch_breed
                # \new_state -> transition new_state []

update state@{page : BreedDetail breed_info@{name : breed_name} _} (ViewBreedDetails breed_requested page_num) | breed_name == breed_requested =
    transition ( state { page = BreedDetail breed_info page_num} ) []

update state (ViewBreedDetails breed_name page_num) =
    let
        working_state =
            navigate_breed_detail_page breed_name page_num
            $ state { busy_work = Just (Detail breed_name) }
    in 
        do
            fork do
                case DM.lookup breed_name state.cache of
                    Nothing ->
                        pure $ BreedDetailsFailed breed_name "breed unknown"
                    Just breed_info ->
                        maybe_fetch_detail breed_info
            pure working_state

update state ViewBreedList =
    transition (state { page = BreedList}) []

{- Below are helper functions to make the the update function shorter. -}
navigate_breed_detail_page :: String -> Int -> State -> State
navigate_breed_detail_page breed_name page_num state =
    state { last_error = Nothing, page = BreedDetail (dummy_breed breed_name) page_num}

maybe_update_page_info :: BreedInfo -> State -> State
maybe_update_page_info breed_info@{name : new_breed_name} state@{page : BreedDetail { name : old_breed_name} page_num} | new_breed_name == old_breed_name =
    state { page = BreedDetail breed_info page_num }
maybe_update_page_info _ state =
    state

maybe_not_busy :: String -> State -> State
maybe_not_busy breed_name state@{ busy_work : Just (Detail busy_breed_name)} | breed_name == busy_breed_name =
    state { busy_work = Nothing }
maybe_not_busy _ state =
    state

dummy_breed :: String -> BreedInfo
dummy_breed = {name : _, cached_image_count : 0, cached_images : [], sub_breeds : []}

maybe_fetch_detail :: BreedInfo -> Aff Message
maybe_fetch_detail {name, cached_images : []} =
    do
        {status, text} <- fetch ("http://localhost:8080/api/breed/" <> name <> "/images") {}
        as_text <- text
        pure $ case status of
            200 ->
                BreedDetailsLoaded name as_text
            _not_200 ->
                BreedDetailsFailed name as_text
maybe_fetch_detail breed_info =
    do
        pure $ BreedDetailsFound breed_info


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

{- Here are a few functions used to parse the json values I get from the server. -}
parse_breed_details :: String -> Either String (Array String)
parse_breed_details text =
    do
      json <- note "json parse failed" $ hush $ Argo.parseJson text
      {message} <- note "json transform failed" $ hush $ decode ( CAR.object "message wrapper" {message : array string} ) json
      pure message

update_initial_list_fetched :: String -> Maybe BreedCache
update_initial_list_fetched string_data =
    do
        json <- hush $ Argo.parseJson string_data
        {message} <- hush $ decode breed_list_codec json
        pure $ mapWithIndex cache_map message

cache_map :: String -> Array String -> BreedInfo
cache_map key value =
    { name : key, sub_breeds : value, cached_images : ([] :: Array String), cached_image_count : 0}

breed_list_codec :: JsonCodec {message :: (DM.Map String (Array String))}
breed_list_codec =
    CAR.object "message wrapper" { message : CAC.strMap (array string) }

{- To help with debugging, it's useful to be able to log data. However, Effect and Aff are
two different things. Thus, to use the Console.logShow (an Effect) in the update function (an Aff),
I use this to transform it.-}
aff_log_show :: forall a. Show a => a -> Aff Unit
aff_log_show thing =
    let
        effect_box = logShow thing
        aff_function = \_ -> (nonCanceler <$ effect_box)
    in
        makeAff aff_function

{- View rendering at last! Take a state and a message generator, and it's off to the races. 
This has the bare minimum styling to keep one's eyes from bleeding. As time passes, pretty and
better functionality can be added. -}
view :: State -> Dispatch Message -> ReactElement
view {busy_work : Just PrimaryList} _ =
    H.div "p-4"
        [ H.text "Loading local cache..." ]
view {busy_work : Just (Detail breed_name)} _dispatch =
    H.div "p-4"
        [ H.text $ "Loading data for breed " <> breed_name ]
view {page : BreedList, last_error : Nothing, cache} dispatch =
    view_breed_list dispatch cache
view {page : BreedList, last_error : Just error, cache} dispatch =
    H.div ""
        [ H.div "error"
            [ H.text error ]
        , view_breed_list dispatch cache
        ]
view {page : BreedDetail breed_info page_num} dispatch =
    H.div "p-4"
        [ H.a_ "" {onClick : dispatch <| ViewBreedList} [ H.text "< back to list" ]
        , H.h1 "" [ H.text breed_info.name ]
        , H.span "" [ H.text $ show breed_info.cached_image_count, H.text " total images" ]
        , H.h2 "" [ H.text "sub breeds"]
        , H.span "" $ map H.text $ DA.intersperse ", " breed_info.sub_breeds
        , view_page_navigator breed_info page_num dispatch
        , H.span "" $ view_images breed_info page_num
        , view_page_navigator breed_info page_num dispatch
        ]

{- TODO make this configurable -}
page_size :: Int
page_size = 20

{- A few helper functions to break up the views. -}
view_images :: BreedInfo -> Int -> Array ReactElement
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

view_breed_list :: (Dispatch Message) -> BreedCache -> ReactElement
view_breed_list dispatch breed_cache =
    foldl (breed_list_entry_fold dispatch) [] breed_cache
    # H.dl ""

breed_list_entry_fold :: (Dispatch Message) -> Array ReactElement -> BreedInfo -> Array ReactElement
breed_list_entry_fold dispatch acc breed_info =
    let
        sub_breed = sub_breed_element breed_info
        breed = breed_element dispatch breed_info
    in
        DA.snoc (DA.snoc acc breed) sub_breed

sub_breed_element :: BreedInfo -> ReactElement
sub_breed_element {sub_breeds} =
    DA.intersperse ", " sub_breeds
    # map H.text
    # H.dd ""

breed_element :: (Dispatch Message) -> BreedInfo -> ReactElement
breed_element dispatch {name} =
    H.text name
    # H.a_ "" {onClick:dispatch <| ViewBreedDetails name 1}
    # H.dt ""

{- And finally the primary entry point! -}
main :: Effect Unit
main = defaultMain { def: { init, view, update}, elementId: "app"}

{- For a more production ready project, I would likely split this module into several different ones.
The BreedInfo type with the json decoders for it are the first things I would pull out. Depending on
how it evolves, the view function may end up as serveral different modules focusing on smaller parts.
-}