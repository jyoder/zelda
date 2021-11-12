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
    , player :: Character
    }

type Character
  = { body :: Body
    , movement :: Movement
    }

type Body
  = { boundary :: Boundary
    , velocity :: Vector
    , force :: Vector
    }

data Movement
  = Movement Direction
  | NoMovement

data Direction
  = North
  | NorthEast
  | East
  | SouthEast
  | South
  | SouthWest
  | West
  | NorthWest

type Boundary
  = { location :: Location
    , dimensions :: Dimensions
    }

type Location
  = Matrix2x1

type Dimensions
  = { width :: Number
    , height :: Number
    }

type Vector
  = Matrix2x1

type Matrix2x1
  = { x :: Number
    , y :: Number
    }

type Controller
  = { up :: ButtonState
    , down :: ButtonState
    , left :: ButtonState
    , right :: ButtonState
    }

data ButtonState
  = NotPressed
  | Pressed

type GameAssets
  = { playerImage :: CanvasImageSource
    }

derive instance eqButtonPosition :: Eq ButtonState

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
  keydownListener <- eventListener (handleControllerEvent Pressed controllerRef)
  keyupListener <- eventListener (handleControllerEvent NotPressed controllerRef)
  addEventListener keydown keydownListener false (toEventTarget window)
  addEventListener keyup keyupListener false (toEventTarget window)
  pure { window, canvas, context2d, controllerRef }

selectCanvas :: String -> String -> Effect CanvasElement
selectCanvas id errorMessage = do
  maybeCanvas <- getCanvasElementById id
  maybe (throwError (error errorMessage)) pure maybeCanvas

loadGameAssets :: Aff GameAssets
loadGameAssets = do
  playerImage <- loadImage "https://images-wixmp-ed30a86b8c4ca887773594c2.wixmp.com/f/b91ae6af-3261-4f95-990e-4896507279ad/d5jzig1-3bd05d51-8646-443c-9030-600bd5eaf473.png?token=eyJ0eXAiOiJKV1QiLCJhbGciOiJIUzI1NiJ9.eyJzdWIiOiJ1cm46YXBwOjdlMGQxODg5ODIyNjQzNzNhNWYwZDQxNWVhMGQyNmUwIiwiaXNzIjoidXJuOmFwcDo3ZTBkMTg4OTgyMjY0MzczYTVmMGQ0MTVlYTBkMjZlMCIsIm9iaiI6W1t7InBhdGgiOiJcL2ZcL2I5MWFlNmFmLTMyNjEtNGY5NS05OTBlLTQ4OTY1MDcyNzlhZFwvZDVqemlnMS0zYmQwNWQ1MS04NjQ2LTQ0M2MtOTAzMC02MDBiZDVlYWY0NzMucG5nIn1dXSwiYXVkIjpbInVybjpzZXJ2aWNlOmZpbGUuZG93bmxvYWQiXX0.js96fjW1bMXaPF6fzpgHOc6xbsL7CsSqU1Nit2OEGwA"
  pure { playerImage }

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

initialGame :: Time -> Game
initialGame time =
  { time
  , viewPort: initialViewPort
  , player: initialPlayer
  }

initialViewPort :: Boundary
initialViewPort =
  { location: { x: 0.0, y: 0.0 }
  , dimensions: defaultViewPortDimensions
  }

defaultViewPortDimensions :: Dimensions
defaultViewPortDimensions = { width: 900.0, height: 600.0 }

initialPlayer :: Character
initialPlayer =
  { body: initialPlayerBody
  , movement: NoMovement
  }

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
  { up: NotPressed
  , down: NotPressed
  , left: NotPressed
  , right: NotPressed
  }

handleControllerEvent :: ButtonState -> Ref Controller -> Event -> Effect Unit
handleControllerEvent buttonState controllerRef event = case fromEvent event of
  Just keyboardEvent -> modify_ (updateController buttonState keyboardEvent) controllerRef
  Nothing -> pure unit

updateController :: ButtonState -> KeyboardEvent -> Controller -> Controller
updateController buttonState keyboardEvent controller = case code keyboardEvent of
  "ArrowUp" -> controller { up = buttonState }
  "ArrowDown" -> controller { down = buttonState }
  "ArrowLeft" -> controller { left = buttonState }
  "ArrowRight" -> controller { right = buttonState }
  _ -> controller

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
    maybeGame = updateGame time controller game
  case maybeGame of
    Just game' -> do
      requestNextFrame gameContext gameAssets game'
      pure unit
    Nothing -> do
      log "Game ended"
      pure unit

readController :: Ref Controller -> Aff Controller
readController controllerRef =
  makeAff
    ( \callback -> do
        controller <- read controllerRef
        callback $ Right controller
        mempty
    )

updateGame :: Time -> Controller -> Game -> Maybe Game
updateGame time controller game =
  game
    # applyControls controller
    # performAi
    # performPhysics time
    # performRules
    # updateTime time
    # evaluateGameEnd

applyControls :: Controller -> Game -> Game
applyControls controller game =
  game
    { player
      { movement = controllerToMovement controller
      }
    }

controllerToMovement :: Controller -> Movement
controllerToMovement { up: Pressed, right: NotPressed, down: NotPressed, left: NotPressed } = Movement North

controllerToMovement { up: Pressed, right: Pressed, down: NotPressed, left: NotPressed } = Movement NorthEast

controllerToMovement { up: NotPressed, right: Pressed, down: NotPressed, left: NotPressed } = Movement East

controllerToMovement { up: NotPressed, right: Pressed, down: Pressed, left: NotPressed } = Movement SouthEast

controllerToMovement { up: NotPressed, right: NotPressed, down: Pressed, left: NotPressed } = Movement South

controllerToMovement { up: NotPressed, right: NotPressed, down: Pressed, left: Pressed } = Movement SouthWest

controllerToMovement { up: NotPressed, right: NotPressed, down: NotPressed, left: Pressed } = Movement West

controllerToMovement _ = NoMovement

performAi :: Game -> Game
performAi game = game

performPhysics :: Time -> Game -> Game
performPhysics time game =
  game
    { player
      { body = updateBody time game.time game.player.body
      }
    }

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

applyForces :: Game -> Game
applyForces game =
  game
    { player
      { body
        { force = movementToForce game.player.movement
        }
      }
    }

movementToForce :: Movement -> Matrix2x1
movementToForce NoMovement = { x: 0.0, y: 0.0 }

movementToForce (Movement North) = { x: 0.0, y: -600.0 }

movementToForce (Movement NorthEast) = { x: 600.0, y: -600.0 }

movementToForce (Movement East) = { x: 600.0, y: 0.0 }

movementToForce (Movement SouthEast) = { x: 600.0, y: 600.0 }

movementToForce (Movement South) = { x: 0.0, y: 600.0 }

movementToForce (Movement SouthWest) = { x: -600.0, y: 600.0 }

movementToForce (Movement West) = { x: -600.0, y: 0.0 }

movementToForce (Movement NorthWest) = { x: -600.0, y: -600.0 }

performRules :: Game -> Game
performRules game = game

updateTime :: Time -> Game -> Game
updateTime time' game = game { time = time' }

evaluateGameEnd :: Game -> Maybe Game
evaluateGameEnd game = Just game

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
          player.body.boundary.location.x
          player.body.boundary.location.y
          player.body.boundary.dimensions.width
          player.body.boundary.dimensions.height
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

addMatrix2x1 :: Matrix2x1 -> Matrix2x1 -> Matrix2x1
addMatrix2x1 m1 m2 = { x: m1.x + m2.x, y: m1.y + m2.y }

elapsedSeconds :: Time -> Time -> Number
elapsedSeconds time1 time2 =
  let
    (Seconds seconds) = diff time1 time2
  in
    seconds
