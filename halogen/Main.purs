module Main where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Time, diff)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Ref (Ref, new, modify_, read)
import Web.Event.Internal.Types (Event)
import Effect.Exception (error)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, code)
import Graphics.Canvas (Context2D, CanvasElement, getContext2D, getCanvasElementById, fillRect, setCanvasDimensions, tryLoadImage, CanvasImageSource, setFillStyle, drawImageScale)
import Web.HTML (window)
import Web.HTML.Window (Window, requestAnimationFrame, toEventTarget)
import Effect.Class.Console (log)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import Effect.Aff (Aff, makeAff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Now (nowTime)

type GameContext
  = { window :: Window
    , canvas :: CanvasElement
    , context2d :: Context2D
    , controllerRef :: Ref Controller
    }

type Game
  = { time :: Time
    , viewPort :: Boundary
    , player :: Body
    }

type Boundary
  = { location :: Location
    , dimensions :: Dimensions
    }

type Body
  = { boundary :: Boundary
    , velocity :: Vector
    , force :: Vector
    }

type Matrix2x1
  = { x :: Number
    , y :: Number
    }

type Location
  = Matrix2x1

type Dimensions
  = { width :: Number
    , height :: Number
    }

type Vector
  = Matrix2x1

type Controller
  = { up :: ButtonPosition
    , down :: ButtonPosition
    , left :: ButtonPosition
    , right :: ButtonPosition
    }

data ButtonPosition
  = UpPosition
  | DownPosition

type GameAssets
  = { playerImage :: CanvasImageSource
    }

derive instance eqButtonPosition :: Eq ButtonPosition

main :: Effect Unit
main = do
  gameContext <- makeGameContext "game-canvas" defaultViewPortDimensions
  launchAff_ do
    gameAssets <- loadGameAssets
    time <- liftEffect $ nowTime
    run gameContext gameAssets (initialGame time)

makeGameContext :: String -> Dimensions -> Effect GameContext
makeGameContext canvasId viewPortDimensions = do
  window <- window
  canvas <- selectCanvas canvasId ("Could not find game canvas by id: " <> canvasId)
  context2d <- getContext2D canvas
  controllerRef <- new initialController
  setCanvasDimensions canvas viewPortDimensions
  keydownListener <- eventListener (handleControllerEvent DownPosition controllerRef)
  keyupListener <- eventListener (handleControllerEvent UpPosition controllerRef)
  addEventListener keydown keydownListener false (toEventTarget window)
  addEventListener keyup keyupListener false (toEventTarget window)
  pure { window, canvas, context2d, controllerRef }

loadGameAssets :: Aff GameAssets
loadGameAssets = do
  playerImage <- loadImage "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/b91ae6af-3261-4f95-990e-4896507279ad/d5jzig1-3bd05d51-8646-443c-9030-600bd5eaf473.png?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7InBhdGgiOiJcL2ZcL2I5MWFlNmFmLTMyNjEtNGY5NS05OTBlLTQ4OTY1MDcyNzlhZFwvZDVqemlnMS0zYmQwNWQ1MS04NjQ2LTQ0M2MtOTAzMC02MDBiZDVlYWY0NzMucG5nIn1dXSwiYXVkIjpbInVybjpzZXJ2aWNlOmZpbGUuZG93bmxvYWQiXX0.js96fjW1bMXaPF6fzpgHOc6xbsL7CsSqU1Nit2OEGwA"
  pure { playerImage }

selectCanvas :: String -> String -> Effect CanvasElement
selectCanvas id errorMessage = do
  maybeCanvas <- getCanvasElementById id
  maybe (throwError (error errorMessage)) pure maybeCanvas

initialGame :: Time -> Game
initialGame time =
  { time
  , viewPort: initialViewPort
  , player: initialPlayerBody
  }

initialViewPort :: Boundary
initialViewPort =
  { location: { x: 0.0, y: 0.0 }
  , dimensions: defaultViewPortDimensions
  }

defaultViewPortDimensions :: Dimensions
defaultViewPortDimensions = { width: 900.0, height: 600.0 }

initialPlayerBody :: Body
initialPlayerBody =
  { boundary:
      { location: { x: 0.0, y: 0.0 }
      , dimensions: { width: 50.0, height: 50.0 }
      }
  , velocity: { x: 0.0, y: 0.0 }
  , force: { x: 0.0, y: 0.0 }
  }

initialController :: Controller
initialController =
  { up: UpPosition
  , down: UpPosition
  , left: UpPosition
  , right: UpPosition
  }

loadImage :: String -> Aff CanvasImageSource
loadImage url = do
  let
    imageAff =
      makeAff
        ( \callback -> do
            tryLoadImage
              url
              ( \maybeImage -> case maybeImage of
                  Nothing -> callback $ Left $ error ("Failed to load image: " <> url)
                  Just image -> callback $ Right image
              )
            mempty
        )
  result <- attempt imageAff
  case result of
    Right image -> pure image
    Left error -> throwError error

handleControllerEvent :: ButtonPosition -> Ref Controller -> Event -> Effect Unit
handleControllerEvent buttonPosition controllerRef event = case fromEvent event of
  Just keyboardEvent -> modify_ (updateController buttonPosition keyboardEvent) controllerRef
  Nothing -> pure unit

updateController :: ButtonPosition -> KeyboardEvent -> Controller -> Controller
updateController buttonPosition keyboardEvent controller = case code keyboardEvent of
  "ArrowUp" -> controller { up = buttonPosition }
  "ArrowDown" -> controller { down = buttonPosition }
  "ArrowLeft" -> controller { left = buttonPosition }
  "ArrowRight" -> controller { right = buttonPosition }
  _ -> controller

readController :: Ref Controller -> Aff Controller
readController controllerRef =
  makeAff
    ( \callback -> do
        controller <- read controllerRef
        callback $ Right controller
        mempty
    )

requestNextFrame :: GameContext -> GameAssets -> Game -> Aff Unit
requestNextFrame gameContext gameAssets game = do
  _ <- liftEffect $ requestAnimationFrame tick' gameContext.window
  pure unit
  where
  tick' = launchAff_ $ run gameContext gameAssets game

run :: GameContext -> GameAssets -> Game -> Aff Unit
run gameContext gameAssets game = do
  render gameContext gameAssets game
  controller <- readController gameContext.controllerRef
  time <- liftEffect nowTime
  let
    maybeGame = performUpdate time controller game
  case maybeGame of
    Just game' -> do
      requestNextFrame gameContext gameAssets game'
      pure unit
    Nothing -> do
      log "Game ended"
      pure unit

render :: GameContext -> GameAssets -> Game -> Aff Unit
render gameContext gameAssets game =
  let
    player = game.player
  in
    liftEffect do
      clearArea gameContext.context2d game.viewPort
      ( drawImageScale
          gameContext.context2d
          gameAssets.playerImage
          player.boundary.location.x
          player.boundary.location.y
          player.boundary.dimensions.width
          player.boundary.dimensions.height
      )

clearArea :: Context2D -> Boundary -> Effect Unit
clearArea context boundary = do
  setFillStyle context "white"
  fillRect context
    { x: boundary.location.x
    , y: boundary.location.y
    , width: boundary.dimensions.width
    , height: boundary.dimensions.height
    }

performUpdate :: Time -> Controller -> Game -> Maybe Game
performUpdate time controller game =
  game
    # performControls controller
    # performAi
    # performPhysics time
    # updateTime time
    # performRules

performControls :: Controller -> Game -> Game
performControls controller game =
  game
    { player
      { force
        { x = toForce controller.left controller.right
        , y = toForce controller.up controller.down
        }
      }
    }
  where
  toForce UpPosition UpPosition = 0.0

  toForce UpPosition DownPosition = 600.0

  toForce DownPosition DownPosition = 0.0

  toForce DownPosition UpPosition = -600.0

performAi :: Game -> Game
performAi game = game

updateBody :: Time -> Time -> Body -> Body
updateBody time1 time2 body =
  body
    { velocity = addMatrix2x1 body.velocity totalForce
    , boundary
      { location = addMatrix2x1 body.boundary.location distance
      }
    }
  where
  totalForce = addMatrix2x1 actingForce frictionalForce

  actingForce =
    { x: body.force.x * elapsed
    , y: body.force.y * elapsed
    }

  frictionalForce =
    { x: -1.0 * mu * body.velocity.x * elapsed
    , y: -1.0 * mu * body.velocity.y * elapsed
    }

  distance =
    { x: body.velocity.x * elapsed
    , y: body.velocity.y * elapsed
    }

  elapsed = elapsedSeconds time1 time2

  mu = 6.0

performPhysics :: Time -> Game -> Game
performPhysics time game =
  game
    { player = updateBody time game.time game.player
    }

addMatrix2x1 :: Matrix2x1 -> Matrix2x1 -> Matrix2x1
addMatrix2x1 m1 m2 = { x: m1.x + m2.x, y: m1.y + m2.y }

elapsedSeconds :: Time -> Time -> Number
elapsedSeconds time1 time2 =
  let
    (Seconds seconds) = diff time1 time2
  in
    seconds

updateTime :: Time -> Game -> Game
updateTime time' game = game { time = time' }

performRules :: Game -> Maybe Game
performRules game = Just game
