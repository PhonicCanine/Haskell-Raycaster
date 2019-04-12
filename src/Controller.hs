module Controller where

import CodeWorld
import Model
import View

import Data.Text (pack, unpack)

-- | Compute the new Model in response to an Event.
handleEvent :: Event -> Model -> Model
handleEvent event m@(Model walls player@(Player loc ang hel wep) ai projectiles) =
  case event of
    KeyPress key
      -- revert to an empty canvas
      | k == "W" -> movePlayerForward m
      | k == "S" -> movePlayerBackward m
      | k == "D" -> movePlayerRight m
      | k == "A" -> movePlayerLeft m
      | k == "E" -> playerLookLeft m
      | k == "Q" -> playerLookRight m

      

      -- ignore other events
      | otherwise -> m
      where k = unpack key
    PointerPress p -> case m of
      _ -> m
    PointerMovement p -> case m of
      _ -> m
    PointerRelease p -> case m of
      _ -> m
    _ -> m


baseAngle :: Double
baseAngle = 2

baseMovementSpeed :: Double
baseMovementSpeed = 0.5

multiplyVector :: Point -> Double -> Point
multiplyVector p by = ((fst p) * by, (snd p) * by)

movePlayerForward :: Model -> Model
movePlayerForward (Model walls (Player loc ang hel wep) ai projectiles) = (Model walls (Player (addPoint loc (multiplyVector (degreeToVector ang) baseMovementSpeed)) ang hel wep) ai projectiles)

movePlayerBackward :: Model -> Model
movePlayerBackward (Model walls (Player loc ang hel wep) ai projectiles) = (Model walls (Player (addPoint loc (multiplyVector (degreeToVector (ang + 180)) baseMovementSpeed)) ang hel wep) ai projectiles)

movePlayerRight :: Model -> Model
movePlayerRight (Model walls (Player loc ang hel wep) ai projectiles) = (Model walls (Player (addPoint loc (multiplyVector (degreeToVector (ang + 90)) baseMovementSpeed)) ang hel wep) ai projectiles)

movePlayerLeft :: Model -> Model
movePlayerLeft (Model walls (Player loc ang hel wep) ai projectiles) = (Model walls (Player (addPoint loc (multiplyVector (degreeToVector (ang + 270)) baseMovementSpeed)) ang hel wep) ai projectiles)

playerLookLeft :: Model -> Model
playerLookLeft (Model walls (Player loc ang hel wep) ai projectiles) = (Model walls (Player loc (ang + baseAngle) hel wep) ai projectiles)

playerLookRight :: Model -> Model
playerLookRight (Model walls (Player loc ang hel wep) ai projectiles) = (Model walls (Player loc (ang - baseAngle) hel wep) ai projectiles)