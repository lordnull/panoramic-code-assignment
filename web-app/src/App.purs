module App (veep, main) where

import Prelude

import Effect
import Effect.Console (log)

main :: Effect Unit
main = log "hi!"

veep :: String -> Effect Unit
veep = log

