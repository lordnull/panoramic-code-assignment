{- In order to complete the dog.ceo assignment, the web-app needs a server
it can talk to. Thus, a very simple 'pass-through' that can also serve
up the app. -}
module Server where

import Prelude hiding ((/))

{- Essentially the first maintained library I found. It worked well enough for my purpose.-}
import HTTPurple (class Generic, RouteDuplex', ServerM, ResponseM, mkRoute, (/), string, segment, path, noArgs, catchAll, headers, ok, ok', serve)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (concat)
import Data.Array ((:))
import Fetch

{- A route for the index, for the app's js, for the list of breeds, for the breed
image list, and anything else I want to put on the file system like css.

Obviously not a production ready solution. Likely a big security hole in that
last one. However, since this is intended to only run on a local machine and
have the ports it's running exposed even to the local network, it's fine for
this purpose. -}
data Route
  = Index
  | App
  | BreedsList
  | BreedImages String
  | AllOthers (Array String)
derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Index": noArgs
  , "App" : path "App.js" noArgs
  , "BreedsList" : "api" / "breeds" / noArgs
  , "BreedImages" : "api" / "breed" / string segment / "images"
  , "AllOthers" : catchAll
  }

{- The site's primary entry point. -}
load_index :: ResponseM
load_index =
  do
    read_file <- readTextFile UTF8 "output/index.html"
    ok read_file

{- The 'catch all' route, attemtping to ready from the file system.
Really only means one can browse to /index.html and it'll still work. -}
load_file :: Array String -> ResponseM
load_file rel_path =
  do
    read_file <- readTextFile UTF8 $ concat ("output" : rel_path)
    ok read_file

{- Serving up the App.js. Not exactly needed since the catch-all
should hit it. However, I find it's helpful to "shine a light" on
core peices like this. -}
load_app :: ResponseM
load_app =
  do
    read_file <- readTextFile UTF8 "output/App.js"
    ok' ( headers {"Content-Type" : "text/javascript"} ) read_file
    
{- One of the two routes that do not have an associated file. In other
words, these are the routes that are proxies for the dog.ceo api. For
this toy, it's lacking any form of authentication or versioning. It's
literally just spitting back to the client what it's getting. -}
load_breed_list =
  do
    let requestUrl = "https://dog.ceo/api/breeds/list/all"
    {text} <- fetch requestUrl {}
    responseText <- text
    ok responseText

load_breed_images breed_name =
  do
    let requestUrl = "https://dog.ceo/api/breed/" <> breed_name <> "/images"
    {text} <- fetch requestUrl {}
    responseText <- text
    ok responseText

{- The core of the server. -}
main :: ServerM
main =
  serve {port : 8080} { route, router }
  where
  router { route: Index } = load_index
  router { route : App } = load_app
  router { route : BreedsList } = load_breed_list
  router { route : BreedImages breed } = load_breed_images breed
  router { route : AllOthers relpath } = load_file relpath
