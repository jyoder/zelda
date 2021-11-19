module Main where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array (length, index, catMaybes)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..), maybe)
import Data.Time (Time, diff)
import Data.Time.Duration (Seconds(..))
import Effect (Effect)
import Effect.Aff (Aff, makeAff, attempt, launchAff_)
import Effect.Class (liftEffect)
import Effect.Class.Console (log)
import Effect.Exception (error)
import Effect.Now (nowTime)
import Effect.Ref (Ref, new, modify_, read)
import Graphics.Canvas (CanvasElement, CanvasImageSource, Context2D, drawImageScale, fillRect, getCanvasElementById, getContext2D, setCanvasDimensions, setFillStyle, tryLoadImage)
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Window (Window, requestAnimationFrame, toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, code)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import Data.Int (floor)
import Data.Foldable (foldr, for_, any)
import Math (abs)

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
    , solids :: Array Body
    }

type Character
  = { body :: Body
    , movement :: Movement
    , animation :: Animation
    , collisions :: Array Collision
    }

type Body
  = { boundary :: Boundary
    , velocity :: Vector
    , force :: Vector
    }

data Movement
  = Movement Walk Jump

data Walk
  = Walking Direction
  | NotWalking

data Jump
  = Jumping { startedAt :: Time }
  | NotJumping

data Direction
  = East
  | West

data Orientation
  = Vertical
  | Horizontal

type Collision
  = { target :: Body
    , overlap :: Overlap
    }

type Overlap
  = Matrix2x1

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
  = { left :: ButtonState
    , right :: ButtonState
    , spacebar :: ButtonState
    }

data ButtonState
  = NotPressed
  | Pressed

type GameAssets
  = { playerAssets :: CharacterAssets
    }

type CharacterAssets
  = { standingSprite :: Sprite
    , walkingLeftSprite :: Sprite
    , walkingRightSprite :: Sprite
    }

type Animation
  = { startedAt :: Time
    , frameRate :: Number
    }

type Sprite
  = { images :: Array CanvasImageSource
    }

derive instance eqButtonPosition :: Eq ButtonState

derive instance eqOrientation :: Eq Orientation

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
  gilletStanding000 <- loadImage "/sprites/gillet-standing-000.png"
  gilletStanding001 <- loadImage "/sprites/gillet-standing-001.png"
  gilletStanding002 <- loadImage "/sprites/gillet-standing-002.png"
  gilletWalkingRight000 <- loadImage "/sprites/gillet-walking-right-000.png"
  gilletWalkingRight001 <- loadImage "/sprites/gillet-walking-right-001.png"
  gilletWalkingRight002 <- loadImage "/sprites/gillet-walking-right-002.png"
  gilletWalkingLeft000 <- loadImage "/sprites/gillet-walking-left-000.png"
  gilletWalkingLeft001 <- loadImage "/sprites/gillet-walking-left-001.png"
  gilletWalkingLeft002 <- loadImage "/sprites/gillet-walking-left-002.png"
  pure
    { playerAssets:
        { standingSprite:
            { images:
                [ gilletStanding000
                , gilletStanding002
                , gilletStanding000
                , gilletStanding002
                , gilletStanding000
                , gilletStanding002
                , gilletStanding001
                ]
            }
        , walkingLeftSprite:
            { images:
                [ gilletWalkingLeft000
                , gilletWalkingLeft001
                , gilletWalkingLeft002
                ]
            }
        , walkingRightSprite:
            { images:
                [ gilletWalkingRight000
                , gilletWalkingRight001
                , gilletWalkingRight002
                ]
            }
        }
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

initialGame :: Time -> Game
initialGame time =
  { time
  , viewPort: initialViewPort
  , player: initialPlayer time
  , solids: initialSolids
  }

initialViewPort :: Boundary
initialViewPort =
  { location: { x: 0.0, y: 0.0 }
  , dimensions: defaultViewPortDimensions
  }

defaultViewPortDimensions :: Dimensions
defaultViewPortDimensions = { width: 900.0, height: 600.0 }

initialPlayer :: Time -> Character
initialPlayer time =
  { body: initialPlayerBody
  , movement: Movement NotWalking NotJumping
  , animation: startAnimation time 6.0
  , collisions: []
  }

initialPlayerBody :: Body
initialPlayerBody =
  { boundary:
      { location: { x: 10.0, y: 0.0 }
      , dimensions: { width: 80.0, height: 74.0 }
      }
  , velocity: { x: 0.0, y: 0.0 }
  , force: { x: 0.0, y: 0.0 }
  }

initialSolids :: Array Body
initialSolids =
  [ { boundary:
        { location: { x: 0.0, y: 500.0 }
        , dimensions: { width: 200.0, height: 10.0 }
        }
    , velocity: { x: 0.0, y: 0.0 }
    , force: { x: 0.0, y: 0.0 }
    }
  , { boundary:
        { location: { x: 0.0, y: 300.0 }
        , dimensions: { width: 10.0, height: 200.0 }
        }
    , velocity: { x: 0.0, y: 0.0 }
    , force: { x: 0.0, y: 0.0 }
    }
  ]

initialController :: Controller
initialController =
  { left: NotPressed
  , right: NotPressed
  , spacebar: NotPressed
  }

handleControllerEvent :: ButtonState -> Ref Controller -> Event -> Effect Unit
handleControllerEvent buttonState controllerRef event = case fromEvent event of
  Just keyboardEvent -> modify_ (updateController buttonState keyboardEvent) controllerRef
  Nothing -> pure unit

updateController :: ButtonState -> KeyboardEvent -> Controller -> Controller
updateController buttonState keyboardEvent controller = case code keyboardEvent of
  "ArrowLeft" -> controller { left = buttonState }
  "ArrowRight" -> controller { right = buttonState }
  "ArrowUp" -> controller { spacebar = buttonState }
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
    # applyAi
    # applyForces
    # applyPhysics time
    # updateCollisions
    # resolveCollisions
    # applyRules
    # updateTime time
    # evaluateGameEnd

applyControls :: Controller -> Game -> Game
applyControls controller game =
  game
    { player
      { movement =
        controllerToMovement
          anyVerticalCollisions
          game.time
          game.player.movement
          controller
      }
    }
  where
  anyVerticalCollisions = any verticalCollision game.player.collisions

  verticalCollision collision = collisionOrientation collision == Vertical

controllerToMovement :: Boolean -> Time -> Movement -> Controller -> Movement
controllerToMovement _ _ (Movement _ (Jumping { startedAt })) { right: NotPressed, left: NotPressed, spacebar: Pressed } = Movement NotWalking (Jumping { startedAt })

controllerToMovement _ _ (Movement _ (Jumping { startedAt })) { right: Pressed, left: NotPressed, spacebar: Pressed } = Movement (Walking East) (Jumping { startedAt })

controllerToMovement _ _ (Movement _ (Jumping { startedAt })) { right: NotPressed, left: Pressed, spacebar: Pressed } = Movement (Walking West) (Jumping { startedAt })

controllerToMovement true time (Movement _ (NotJumping)) { right: NotPressed, left: NotPressed, spacebar: Pressed } = Movement NotWalking (Jumping { startedAt: time })

controllerToMovement true time (Movement _ (NotJumping)) { right: Pressed, left: NotPressed, spacebar: Pressed } = Movement (Walking East) (Jumping { startedAt: time })

controllerToMovement true time (Movement _ (NotJumping)) { right: NotPressed, left: Pressed, spacebar: Pressed } = Movement (Walking West) (Jumping { startedAt: time })

controllerToMovement _ _ _ { right: NotPressed, left: NotPressed, spacebar: NotPressed } = Movement NotWalking NotJumping

controllerToMovement _ _ _ { right: Pressed, left: NotPressed, spacebar: NotPressed } = Movement (Walking East) NotJumping

controllerToMovement _ _ _ { right: NotPressed, left: Pressed, spacebar: NotPressed } = Movement (Walking West) NotJumping

controllerToMovement _ _ _ _ = Movement NotWalking NotJumping

applyAi :: Game -> Game
applyAi game = game

applyForces :: Game -> Game
applyForces game =
  game
    { player
      { body
        { force = movementToForce game.time game.player.movement
        }
      }
    }

movementToForce :: Time -> Movement -> Matrix2x1
movementToForce time (Movement NotWalking (Jumping { startedAt })) = { x: 0.0, y: -(decideJumpForce startedAt time) }

movementToForce time (Movement (Walking East) (Jumping { startedAt })) = { x: movementForce, y: -(decideJumpForce startedAt time) }

movementToForce time (Movement (Walking West) (Jumping { startedAt })) = { x: -movementForce, y: -(decideJumpForce startedAt time) }

movementToForce _ (Movement (Walking East) NotJumping) = { x: movementForce, y: 0.0 }

movementToForce _ (Movement (Walking West) NotJumping) = { x: -movementForce, y: 0.0 }

movementToForce _ _ = { x: 0.0, y: 0.0 }

movementForce :: Number
movementForce = 1500.0

decideJumpForce :: Time -> Time -> Number
decideJumpForce startedAt now =
  if elapsedSeconds now startedAt < jumpImpulseDuration then
    jumpForce
  else
    0.0

jumpImpulseDuration :: Number
jumpImpulseDuration = 90.0 / 1000.0

jumpForce :: Number
jumpForce = 25000.0

applyPhysics :: Time -> Game -> Game
applyPhysics time game =
  game
    { player
      { body = updateBody time game.time game.player.body
      }
    }

updateBody :: Time -> Time -> Body -> Body
updateBody time1 time2 body =
  body
    { velocity = velocity'
    , boundary
      { location = location'
      }
    }
  where
  location' = addMatrix2x1 body.boundary.location distanceTraveled

  distanceTraveled =
    { x: velocity'.x * elapsed
    , y: velocity'.y * elapsed
    }

  velocity' = addMatrix2x1 body.velocity force'

  force' = foldr addMatrix2x1 actingForce [ frictionForce, gravityForce ]

  actingForce =
    { x: body.force.x * elapsed
    , y: body.force.y * elapsed
    }

  gravityForce =
    { x: 0.0
    , y: 3500.0 * elapsed
    }

  frictionForce =
    { x: -1.0 * mu * body.velocity.x * elapsed
    , y: -1.0 * mu * body.velocity.y * elapsed
    }

  elapsed = elapsedSeconds time1 time2

  mu = 6.0

updateCollisions :: Game -> Game
updateCollisions game =
  game
    { player
      { collisions = detectCollisions game.solids game.player.body
      }
    }

resolveCollisions :: Game -> Game
resolveCollisions game =
  game
    { player
      { body
        { boundary { location = playerLocation' }
        }
      }
    }
  where
  playerLocation' =
    foldr
      addMatrix2x1
      game.player.body.boundary.location
      (collisionResolution <$> game.player.collisions)

collisionResolution :: Collision -> Matrix2x1
collisionResolution collision' =
  if (abs collision'.overlap.x) < (abs collision'.overlap.y) then
    { x: collision'.overlap.x * -1.0, y: 0.0 }
  else
    { x: 0.0, y: collision'.overlap.y * -1.0 }

detectCollisions :: Array Body -> Body -> Array Collision
detectCollisions bodies body = catMaybes $ detectCollision body <$> bodies

detectCollision :: Body -> Body -> Maybe Collision
detectCollision projectile target = case { x: overlapX, y: overlapY } of
  { x: Just x, y: Just y } -> Just { target, overlap: { x, y } }
  _ -> Nothing
  where
  overlapX = overlap px1 px2 tx1 tx2

  overlapY = overlap py1 py2 ty1 ty2

  px1 = projectile.boundary.location.x

  px2 = px1 + projectile.boundary.dimensions.width

  py1 = projectile.boundary.location.y

  py2 = py1 + projectile.boundary.dimensions.height

  tx1 = target.boundary.location.x

  tx2 = tx1 + target.boundary.dimensions.width

  ty1 = target.boundary.location.y

  ty2 = ty1 + target.boundary.dimensions.height

overlap :: Number -> Number -> Number -> Number -> Maybe Number
overlap aLeft aRight bLeft bRight
  | aLeft <= bLeft && bLeft <= aRight && aRight <= bRight = Just (aRight - bLeft)
  | bLeft <= aLeft && aLeft <= aRight && aRight <= bRight = Just (if (aLeft - bLeft) < (bRight - aRight) then aRight - bLeft else bRight - aLeft)
  | aLeft <= bLeft && bLeft <= bRight && bRight <= aRight = Just (if (bLeft - aLeft) < (aRight - bRight) then bRight - aLeft else aRight - bLeft)
  | bLeft <= aLeft && aLeft <= bRight && bRight <= aRight = Just (aLeft - bRight)
  | otherwise = Nothing

collisionOrientation :: Collision -> Orientation
collisionOrientation collision =
  if y > 0.0 && y <= x then
    Vertical
  else
    Horizontal
  where
  x = abs collision.overlap.x

  y = abs collision.overlap.y

applyRules :: Game -> Game
applyRules game = game

updateTime :: Time -> Game -> Game
updateTime time' game = game { time = time' }

evaluateGameEnd :: Game -> Maybe Game
evaluateGameEnd game = Just game

render :: GameContext -> GameAssets -> Game -> Aff Unit
render gameContext gameAssets game =
  liftEffect do
    clearArea gameContext.context2d game.viewPort
    renderSolids gameContext game
    renderPlayer gameContext gameAssets game

renderSolids :: GameContext -> Game -> Effect Unit
renderSolids gameContext game =
  for_
    game.solids
    (renderSolid gameContext.context2d)

renderSolid :: Context2D -> Body -> Effect Unit
renderSolid context2d body = do
  setFillStyle context2d "red"
  fillRect
    context2d
    { x: body.boundary.location.x
    , y: body.boundary.location.y
    , width: body.boundary.dimensions.width
    , height: body.boundary.dimensions.height
    }

renderPlayer :: GameContext -> GameAssets -> Game -> Effect Unit
renderPlayer gameContext gameAssets game =
  renderSprite
    gameContext.context2d
    game.time
    game.player.body
    game.player.animation
    (playerSprite gameAssets game.player.movement)

playerSprite :: GameAssets -> Movement -> Sprite
playerSprite gameAssets (Movement (Walking West) _) = gameAssets.playerAssets.walkingLeftSprite

playerSprite gameAssets (Movement (Walking East) _) = gameAssets.playerAssets.walkingRightSprite

playerSprite gameAssets _ = gameAssets.playerAssets.standingSprite

renderSprite :: Context2D -> Time -> Body -> Animation -> Sprite -> Effect Unit
renderSprite context2d time body animation sprite = case maybeCurrentImage of
  Just currentImage ->
    drawImageScale
      context2d
      currentImage
      body.boundary.location.x
      body.boundary.location.y
      body.boundary.dimensions.width
      body.boundary.dimensions.height
  Nothing -> pure unit
  where
  maybeCurrentImage = index sprite.images frameIndex

  frameIndex = mod currentFrame frameCount

  currentFrame = floor $ (elapsedSeconds time animation.startedAt) * animation.frameRate

  frameCount = length sprite.images

clearArea :: Context2D -> Boundary -> Effect Unit
clearArea context boundary = do
  setFillStyle context "white"
  fillRect context
    { x: boundary.location.x
    , y: boundary.location.y
    , width: boundary.dimensions.width
    , height: boundary.dimensions.height
    }

startAnimation :: Time -> Number -> Animation
startAnimation startedAt frameRate = { startedAt, frameRate }

addMatrix2x1 :: Matrix2x1 -> Matrix2x1 -> Matrix2x1
addMatrix2x1 m1 m2 = { x: m1.x + m2.x, y: m1.y + m2.y }

elapsedSeconds :: Time -> Time -> Number
elapsedSeconds time1 time2 =
  let
    (Seconds seconds) = diff time1 time2
  in
    seconds
