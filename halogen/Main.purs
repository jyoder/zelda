module Main where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Maybe (Maybe(..), maybe)
import Effect (Effect)
import Effect.Exception (error)
import Graphics.Canvas (Context2D, CanvasElement, getContext2D, getCanvasElementById, fillRect)
import Web.HTML (window)
import Web.HTML.Window (Window, requestAnimationFrame)
import Effect.Class.Console (log)

type Game
  = { x :: Number
    , y :: Number
    }

data Controller
  = Controller

main :: Effect Unit
main = do
  window <- window
  canvas <- selectCanvas "canvas" "Could not find canvas (#canvas)."
  context <- getContext2D canvas
  tick window canvas context initialGame
  pure unit

selectCanvas :: String -> String -> Effect CanvasElement
selectCanvas id errorMessage = do
  maybeCanvas <- getCanvasElementById id
  maybe (throwError (error errorMessage)) pure maybeCanvas

initialGame :: Game
initialGame = { x: 0.0, y: 0.0 }

tick :: Window -> CanvasElement -> Context2D -> Game -> Effect Unit
tick window canvas context game = do
  render context game
  controller <- readController
  let
    maybeGame = performUpdate controller game
  case maybeGame of
    Just game' -> do
      _ <- requestAnimationFrame (tick window canvas context game') window
      pure unit
    Nothing -> do
      log "Game ended"
      pure unit

render :: Context2D -> Game -> Effect Unit
render context game = do
  fillRect context { x: game.x, y: game.y, width: 50.0, height: 50.0 }

readController :: Effect Controller
readController = pure Controller

performUpdate :: Controller -> Game -> Maybe Game
performUpdate controller game =
  game
    # performControl controller
    # performAi
    # performPhysics
    # performRules

performControl :: Controller -> Game -> Game
performControl controller game = game

performAi :: Game -> Game
performAi game = game

performPhysics :: Game -> Game
performPhysics game = game { x = game.x + 1.0 }

performRules :: Game -> Maybe Game
performRules game = Just game
