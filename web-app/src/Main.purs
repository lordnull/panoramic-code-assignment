module Main where

import Prelude hiding ((/))

import HTTPurple (class Generic, RouteDuplex', ServerM, mkRoute, noArgs, ok, serve)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

data Route
  = Index
--  | BreedsList
--  | BreedImages String
--  | AllOthers (Array String)
derive instance Generic Route _

route :: RouteDuplex' Route
route = mkRoute
  { "Index": noArgs
  --, "BreedsList" : "api" / "breeds"
  --, "BreedImages" : "api" / "breed" / string segment / "images"
  --, "AllOthers" : catchAll
  }

load_index =
  do
    read_file <- readTextFile UTF8 "output/index.html"
    ok read_file
{-
load_file rel_path =
  ok $ readTextFile UTF8 $ concat ("output" : rel_path)


load_breed_list =
  ok "breed list"

load_breed_images =
  ok "images"-}

main :: ServerM
main =
  serve {port : 8080} { route, router }
  where
  --router { route: Index } = load_index 
  router { route: Index } = load_index
  --router { route : BreedsList } = load_breed_list
  --router { route : BreedImages breed } = load_breed_images breed
  --router { route : AllOthers relpath } = load_file relpath
