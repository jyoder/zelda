module Main where

import Prelude
import Control.Monad.Error.Class (throwError)
import Data.Array (length, index, catMaybes, range)
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
import Graphics.Canvas as C
import Web.Event.EventTarget (eventListener, addEventListener)
import Web.Event.Internal.Types (Event)
import Web.HTML (window)
import Web.HTML.Window (Window, requestAnimationFrame, toEventTarget)
import Web.UIEvent.KeyboardEvent (KeyboardEvent, fromEvent, code)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)
import Data.Int (floor, toNumber)
import Data.Foldable (foldr, for_, any)
import Math (abs)

type GameContext
  = { window :: Window
    , canvas :: C.CanvasElement
    , context2d :: C.Context2D
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

type Movement
  = { walk :: Walk, jump :: Jump
    }

data Walk
  = Walking Direction
  | NotWalking

data Jump
  = Jumping { startedAt :: Time }
  | Falling
  | Landing
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
    , up :: ButtonState
    }

data ButtonState
  = NotPressed
  | Pressed

type GameAssets
  = { player :: CharacterAssets
    , solids :: SolidAssets
    }

type SolidAssets
  = { blockSolid :: Sprite
    }

type CharacterAssets
  = { standing :: Sprite
    , walkingLeft :: Sprite
    , walkingRight :: Sprite
    , jumpingLeft :: Sprite
    , jumpingRight :: Sprite
    , falling :: Sprite
    }

type Animation
  = { startedAt :: Time
    , frameRate :: Number
    }

type Sprite
  = { images :: Array C.CanvasImageSource
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
  context2d <- C.getContext2D canvas
  controllerRef <- new initialController
  C.setCanvasDimensions canvas viewPortDimensions
  keydownListener <- eventListener (handleControllerEvent Pressed controllerRef)
  keyupListener <- eventListener (handleControllerEvent NotPressed controllerRef)
  addEventListener keydown keydownListener false (toEventTarget window)
  addEventListener keyup keyupListener false (toEventTarget window)
  pure { window, canvas, context2d, controllerRef }

selectCanvas :: String -> String -> Effect C.CanvasElement
selectCanvas id errorMessage = do
  maybeCanvas <- C.getCanvasElementById id
  maybe (throwError (error errorMessage)) pure maybeCanvas

loadGameAssets :: Aff GameAssets
loadGameAssets = do
  blockSolid000 <- loadImage "/sprites/block-solid-000.png"
  gilletStanding000 <- loadImage "/sprites/gillet-standing-000.png"
  gilletStanding001 <- loadImage "/sprites/gillet-standing-001.png"
  gilletStanding002 <- loadImage "/sprites/gillet-standing-002.png"
  gilletWalkingRight000 <- loadImage "/sprites/gillet-walking-right-000.png"
  gilletWalkingRight001 <- loadImage "/sprites/gillet-walking-right-001.png"
  gilletWalkingRight002 <- loadImage "/sprites/gillet-walking-right-002.png"
  gilletWalkingLeft000 <- loadImage "/sprites/gillet-walking-left-000.png"
  gilletWalkingLeft001 <- loadImage "/sprites/gillet-walking-left-001.png"
  gilletWalkingLeft002 <- loadImage "/sprites/gillet-walking-left-002.png"
  gilletJumpingLeft000 <- loadImage "/sprites/gillet-jumping-left-000.png"
  gilletJumpingRight000 <- loadImage "/sprites/gillet-jumping-right-000.png"
  gilletFalling000 <- loadImage "/sprites/gillet-falling-000.png"
  pure
    { player:
        { standing:
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
        , walkingLeft:
            { images:
                [ gilletWalkingLeft000
                , gilletWalkingLeft001
                , gilletWalkingLeft002
                ]
            }
        , walkingRight:
            { images:
                [ gilletWalkingRight000
                , gilletWalkingRight001
                , gilletWalkingRight002
                ]
            }
        , jumpingLeft:
            { images:
                [ gilletJumpingLeft000 ]
            }
        , jumpingRight:
            { images:
                [ gilletJumpingRight000 ]
            }
        , falling:
            { images:
                [ gilletFalling000 ]
            }
        }
    , solids:
        { blockSolid:
            { images: [ blockSolid000 ]
            }
        }
    }

loadImage :: String -> Aff C.CanvasImageSource
loadImage url = do
  let
    imageAff =
      makeAff
        ( \callback -> do
            C.tryLoadImage
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
  , movement: { walk: NotWalking, jump: NotJumping }
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
  ( \i ->
      { boundary:
          { location: { x: (toNumber (mod i 24)) * 40.0, y: 500.0 + (toNumber (floor ((toNumber i) / 24.0)) * 37.0) }
          , dimensions: { width: 40.0, height: 37.0 }
          }
      , velocity: { x: 0.0, y: 0.0 }
      , force: { x: 0.0, y: 0.0 }
      }
  )
    <$> range 0 (24 * 4)

initialController :: Controller
initialController =
  { left: NotPressed
  , right: NotPressed
  , up: NotPressed
  }

handleControllerEvent :: ButtonState -> Ref Controller -> Event -> Effect Unit
handleControllerEvent buttonState controllerRef event = case fromEvent event of
  Just keyboardEvent -> modify_ (updateController buttonState keyboardEvent) controllerRef
  Nothing -> pure unit

updateController :: ButtonState -> KeyboardEvent -> Controller -> Controller
updateController buttonState keyboardEvent controller = case code keyboardEvent of
  "ArrowLeft" -> controller { left = buttonState }
  "ArrowRight" -> controller { right = buttonState }
  "ArrowUp" -> controller { up = buttonState }
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
        movement'
          { controller
          , onFloor
          , now: game.time
          , movement: game.player.movement
          }
      }
    }
  where
  onFloor = any verticalCollision game.player.collisions

  verticalCollision collision = collisionOrientation collision == Vertical

movement' :: { controller :: Controller, onFloor :: Boolean, now :: Time, movement :: Movement } -> Movement
movement' { controller, onFloor, now, movement } =
  { walk:
      walk'
        { leftButton: controller.left, rightButton: controller.right
        }
  , jump:
      jump'
        { jumpButton: controller.up, onFloor, now, jump: movement.jump
        }
  }

walk' :: { leftButton :: ButtonState, rightButton :: ButtonState } -> Walk
walk' { leftButton: Pressed, rightButton: NotPressed } = Walking West

walk' { leftButton: NotPressed, rightButton: Pressed } = Walking East

walk' _ = NotWalking

jump' :: { jumpButton :: ButtonState, onFloor :: Boolean, now :: Time, jump :: Jump } -> Jump
jump' { jumpButton: Pressed, onFloor: true, now, jump: NotJumping } = Jumping { startedAt: now }

jump' { jumpButton: Pressed, onFloor: false, now, jump: Jumping { startedAt } } =
  if elapsedSeconds now startedAt < jumpImpulseDuration then
    Jumping { startedAt }
  else
    Falling

jump' { jumpButton: Pressed, onFloor: false, now: _, jump: Falling } = Falling

jump' { jumpButton: Pressed, onFloor: true, now: _, jump: Falling } = Landing

jump' { jumpButton: Pressed, onFloor: true, now: _, jump: Landing } = Landing

jump' { jumpButton: NotPressed, onFloor: false, now: _, jump: _ } = Falling

jump' _ = NotJumping

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
movementToForce time { walk: NotWalking, jump: Jumping { startedAt } } = { x: 0.0, y: -(decideJumpForce startedAt time) }

movementToForce time { walk: Walking East, jump: Jumping { startedAt } } = { x: movementForce, y: -(decideJumpForce startedAt time) }

movementToForce time { walk: Walking West, jump: Jumping { startedAt } } = { x: -movementForce, y: -(decideJumpForce startedAt time) }

movementToForce _ { walk: Walking East, jump: _ } = { x: movementForce, y: 0.0 }

movementToForce _ { walk: Walking West, jump: _ } = { x: -movementForce, y: 0.0 }

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
jumpImpulseDuration = 70.0 / 1000.0

jumpForce :: Number
jumpForce = 30000.0

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
    , y: 4000.0 * elapsed
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
    renderSolids gameContext gameAssets game
    renderPlayer gameContext gameAssets game

renderSolids :: GameContext -> GameAssets -> Game -> Effect Unit
renderSolids gameContext gameAssets game =
  for_
    game.solids
    (renderSolid gameContext gameAssets game)

renderSolid :: GameContext -> GameAssets -> Game -> Body -> Effect Unit
renderSolid gameContext gameAssets game body = do
  renderSprite
    gameContext.context2d
    game.time
    body
    { startedAt: game.time, frameRate: 1.0 }
    gameAssets.solids.blockSolid

renderPlayer :: GameContext -> GameAssets -> Game -> Effect Unit
renderPlayer gameContext gameAssets game =
  renderSprite
    gameContext.context2d
    game.time
    game.player.body
    game.player.animation
    (playerSprite gameAssets game.player.movement)

playerSprite :: GameAssets -> Movement -> Sprite
playerSprite gameAssets { walk: Walking East, jump: NotJumping } = gameAssets.player.walkingRight

playerSprite gameAssets { walk: Walking West, jump: NotJumping } = gameAssets.player.walkingLeft

playerSprite gameAssets { walk: Walking East, jump: Landing } = gameAssets.player.walkingRight

playerSprite gameAssets { walk: Walking West, jump: Landing } = gameAssets.player.walkingLeft

playerSprite gameAssets { walk: Walking East, jump: Jumping _ } = gameAssets.player.jumpingRight

playerSprite gameAssets { walk: Walking West, jump: Jumping _ } = gameAssets.player.jumpingLeft

playerSprite gameAssets { walk: _, jump: Falling } = gameAssets.player.falling

playerSprite gameAssets _ = gameAssets.player.standing

renderSprite :: C.Context2D -> Time -> Body -> Animation -> Sprite -> Effect Unit
renderSprite context2d time body animation sprite = case maybeCurrentImage of
  Just currentImage ->
    C.drawImageScale
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

clearArea :: C.Context2D -> Boundary -> Effect Unit
clearArea context boundary = do
  C.setFillStyle context "white"
  C.fillRect context
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
