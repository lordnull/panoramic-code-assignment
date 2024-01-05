module Server where

import Prelude hiding ((/))

import HTTPurple (class Generic, RouteDuplex', ServerM, ResponseM, mkRoute, (/), path, noArgs, catchAll, headers, ok, ok', serve)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)
import Node.Path (concat)
import Data.Array ((:))

data Route
  = Index
  | App
--  | BreedsList
--  | BreedImages String
  | AllOthers (Array String)
derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Index": noArgs
  , "App" : path "App.js" noArgs
  --, "BreedsList" : "api" / "breeds"
  --, "BreedImages" : "api" / "breed" / string segment / "images"
  , "AllOthers" : catchAll
  }

load_index :: ResponseM
load_index =
  do
    read_file <- readTextFile UTF8 "output/index.html"
    ok read_file

load_file :: Array String -> ResponseM
load_file rel_path =
  do
    read_file <- readTextFile UTF8 $ concat ("output" : rel_path)
    ok read_file

load_app :: ResponseM
load_app =
  do
    read_file <- readTextFile UTF8 "output/App.js"
    ok' ( headers {"Content-Type" : "text/javascript"} ) read_file
    
{-
load_breed_list =
  ok "breed list"

load_breed_images =
  ok "images"-}

main :: ServerM
main =
  serve {port : 8080} { route, router }
  where
  router { route: Index } = load_index
  router { route : App } = load_app
  --router { route : BreedsList } = load_breed_list
  --router { route : BreedImages breed } = load_breed_images breed
  router { route : AllOthers relpath } = load_file relpath
