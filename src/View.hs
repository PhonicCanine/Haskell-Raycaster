module View where

import CodeWorld
import Data.Text (pack)
import Model

modelToPicture :: Model -> Picture
modelToPicture m
  = scaled 0.2 0.2 (renderModel m)
  -- & coordinatePlane
  where
    stringToText = text . pack

horizontalResolution :: Integer
horizontalResolution = 100

verticalResolution :: Integer
verticalResolution = 50

horizontalFieldOfView :: Integer
horizontalFieldOfView = 50

degreesPerScanline :: Double
degreesPerScanline = (fromIntegral horizontalFieldOfView) / (fromIntegral horizontalResolution)

defaultColour :: Colour
defaultColour = azure

startingOffsetDegrees :: Double
startingOffsetDegrees = -(((fromIntegral horizontalResolution) * degreesPerScanline) / 2)

endingOffsetDegrees :: Double
endingOffsetDegrees = (((fromIntegral horizontalResolution) * degreesPerScanline) / 2)

viewDistance :: Integer
viewDistance = 40

renderModel :: Model -> Picture
renderModel (Model walls (Player location angle _ _) _ _) = renderMap walls location angle

renderMap :: [Wall] -> Point -> Double -> Picture
renderMap walls from angle = traceRays 0
    where
      traceRays :: Integer -> Picture
      traceRays hCount
        | hCount >= horizontalResolution = translated (fromIntegral hCount) 0 (formVerticalLine (startingOffsetDegrees + angle + ((fromIntegral hCount) * degreesPerScanline)))
        | otherwise = translated (fromIntegral hCount) 0 (formVerticalLine (startingOffsetDegrees + angle + ((fromIntegral hCount) * degreesPerScanline))) & (traceRays (hCount + 1))

      --takes angle in degrees
      formVerticalLine :: Double -> Picture
      formVerticalLine ang = case (traceHelper 0 from (degreeToVector ang) walls) of
        args@(Just _, (_, _)) -> (makeActualLine args 0) & (coloured defaultColour (solidRectangle 1 (fromIntegral verticalResolution)))
        (Nothing, _) -> (coloured defaultColour (solidRectangle 1 (fromIntegral verticalResolution)))

      makeActualLine :: (Maybe Wall, (Integer, Point)) -> Integer -> Picture
      --makeActualLine args@(Just w@(Wall _ _ t), (dist, point)) count = (translated 0 0 (coloured (darker (1 - (findYScaleGivenDistance (fromIntegral dist))) cyan) (solidRectangle 1 (fromIntegral height))))
      makeActualLine args@(Just w@(Wall _ _ t), (dist, point)) count
       | count <= floor ((fromIntegral verticalResolution) * (findYScaleGivenDistance (fromIntegral dist))) = case (getColourAtPointOnTexture (getTextureAtIndex t) ((findRelativeXPosOfPointOnWall point w), ((fromIntegral count) / (fromIntegral height)))) of
           Nothing -> blank
           Just c -> (translated 0 (((fromIntegral height) / 2) - (fromIntegral count)) (coloured (darker (1 - (findYScaleGivenDistance (fromIntegral dist))) c) (solidRectangle 1 1))) & (makeActualLine args (count + 1))
       | otherwise = blank
         where
           height :: Integer
           height = (floor ((fromIntegral verticalResolution) * (findYScaleGivenDistance (fromIntegral dist))))
      
traceHelper :: Integer -> Point -> Point -> [Wall] -> (Maybe Wall, (Integer, Point))
traceHelper iteration currentPoint vec walls = case (isPointInsideAWall walls currentPoint) of
  (False, Nothing)
    | iteration > viewDistance -> (Nothing, (iteration, currentPoint))
    | iteration <= viewDistance -> traceHelper (iteration  + 1) (addPoint currentPoint vec) vec walls
  (True, z) -> (z, (iteration, currentPoint))
  _ -> (Nothing, (iteration, currentPoint))

-- a texture atlas is used, and this just looks up textures in that atlas
getTextureAtIndex :: Int -> Texture
getTextureAtIndex idx = textureIndexHelper 0 idx builtInTextures
  where
    textureIndexHelper :: Int -> Int -> [Texture] -> Texture
    textureIndexHelper i aim textures = case textures of
      [] -> defaultTexture -- show the last texture if no other texture has been right
      x:xs
        | i == aim -> x
        | i < aim -> textureIndexHelper (i + 1) aim xs
        | otherwise -> error "Dunno how this happened :/"

-- This controls all the changes that happen to rendering at a distance
findYScaleGivenDistance :: Double -> Double
findYScaleGivenDistance d = ((fromIntegral viewDistance) - d) / (fromIntegral viewDistance)

findRelativeXPosOfPointOnWall :: Point -> Wall -> Double
findRelativeXPosOfPointOnWall p w = case findDirectionalOffsetOfPointFromWallCentre p w of
  -- We have cast to the top of the wall
  (0,1) -> findXOffsetPercOnWall p w
  -- We have cast to the bottom of the wall
  (0,(-1)) -> (1.0 - (findXOffsetPercOnWall p w))
  -- We have cast to the left of the wall
  ((-1),0) -> (findYOffsetPercOnWall p w)
  -- We have cast to the right of the wall
  ((1),0) -> (1.0 - (findYOffsetPercOnWall p w))
  _ -> 0

findYOffsetPercOnWall :: Point -> Wall -> Double
findYOffsetPercOnWall p (Wall p1 p2 _) = (snd p - (min (snd p1) (snd p2))) / (findHeightBetweenPoints p1 p2)

findXOffsetPercOnWall :: Point -> Wall -> Double
findXOffsetPercOnWall p (Wall p1 p2 _) = (fst p - (min (fst p1) (fst p2))) / (findWidthBetweenPoints p1 p2)

findHeightBetweenPoints :: Point -> Point -> Double
findHeightBetweenPoints p1 p2 = abs ((snd p1) - (snd p2))

findWidthBetweenPoints :: Point -> Point -> Double
findWidthBetweenPoints p1 p2 = abs ((fst p1) - (fst p2))

--Draw triangles in each direction to figure out which side of the wall we've hit
findDirectionalOffsetOfPointFromWallCentre :: Point -> Wall -> Point
findDirectionalOffsetOfPointFromWallCentre point (Wall p1 p2 _)
  | pointIsInTriangleBetween tl tr = (0,1)
  | pointIsInTriangleBetween tr br = (1,0)
  | pointIsInTriangleBetween br bl = (0,(-1))
  | otherwise = ((-1),0)

    where
      c = findCentrePoint p1 p2

      tl = getTopLeftBound p1 p2
      tr = getTopRightBound p1 p2
      bl = getBottomLeftBound p1 p2
      br = getBottomRightBound p1 p2

      pointIsInTriangleBetween :: Point -> Point -> Bool
      pointIsInTriangleBetween b1 b2 = isPointInTriangleDefinedByPoints b1 b2 c point

getTopLeftBound :: Point -> Point -> Point
getTopLeftBound b1 b2 = ((min (fst b1) (fst b2)) , (max (snd b1) (snd b2)))

getTopRightBound :: Point -> Point -> Point
getTopRightBound b1 b2 = ((max (fst b1) (fst b2)) , (max (snd b1) (snd b2)))

getBottomLeftBound :: Point -> Point -> Point
getBottomLeftBound b1 b2 = ((min (fst b1) (fst b2)) , (min (snd b1) (snd b2)))

getBottomRightBound :: Point -> Point -> Point
getBottomRightBound b1 b2 = ((max (fst b1) (fst b2)) , (min (snd b1) (snd b2)))

findCentrePoint :: Point -> Point -> Point
findCentrePoint p1 p2 = (((fst p1) + (fst p2)) / 2 , ((snd p1) + (snd p2)) / 2)

-- https://stackoverflow.com/questions/2049582/how-to-determine-if-a-point-is-in-a-2d-triangle
isPointInTriangleDefinedByPoints :: Point -> Point -> Point -> Point -> Bool
isPointInTriangleDefinedByPoints v1 v2 v3 pt
  | (has_neg && has_pos) = False
  | otherwise = True
    where
      
      d1 = sign pt v1 v2
      d2 = sign pt v2 v3
      d3 = sign pt v3 v1

      has_neg = ((d1 < 0) || (d2 < 0) || (d3 < 0))
      has_pos = ((d1 > 0) || (d2 > 0) || (d3 > 0))

      sign :: Point -> Point -> Point -> Double
      sign p1 p2 p3 = (((fst p1) - (fst p3)) * ((snd p2) - (snd p3))) - (((fst p2) - (fst p3)) * ((snd p1) - (snd p3)))

addPoint :: Point -> Point -> Point
addPoint p1 p2 = ((fst p1) + (fst p2), (snd p1) + (snd p2))


isPointInsideAWall :: [Wall]->Point->(Bool, Maybe Wall)
isPointInsideAWall walls p = case walls of
  [] -> (False, Nothing)
  x:xs -> if isPointInsideWall x p then (True, Just x) else isPointInsideAWall xs p

isPointInsideWall :: Wall -> Point -> Bool
isPointInsideWall (Wall p1 p2 _) p = doBoundsContainPoint p1 p2 p

asRad :: Double -> Double
asRad a = (a / 180) * pi

degreeToVector :: Double -> Point
degreeToVector a = ((sin (asRad a)), (cos (asRad a)))

getColourAtPointOnTexture :: Texture -> Point -> Maybe Colour
getColourAtPointOnTexture texture point = case texture of
  Texture [] -> Nothing
  Texture (x:xs) -> case getColourOfSubregion x point of
    Just c -> Just c
    Nothing -> getColourAtPointOnTexture (Texture xs) point
    where
      getColourOfSubregion :: ColouredRegion -> Point -> Maybe Colour
      getColourOfSubregion (ColouredRegion colour (Region p1 p2)) pt
        | doBoundsContainPoint p1 p2 pt = Just colour
        | otherwise = Nothing

doBoundsContainPoint :: Point -> Point -> Point -> Bool
doBoundsContainPoint b1 b2 p
  | (fst b1) <= (fst p) && (fst b2) >= (fst p) && (snd b1) <= (snd p) && (snd b2) >= (snd p) = True
  | (fst b2) <= (fst p) && (fst b1) >= (fst p) && (snd b1) <= (snd p) && (snd b2) >= (snd p) = True
  | (fst b1) <= (fst p) && (fst b2) >= (fst p) && (snd b2) <= (snd p) && (snd b1) >= (snd p) = True
  | (fst b2) <= (fst p) && (fst b1) >= (fst p) && (snd b2) <= (snd p) && (snd b1) >= (snd p) = True
  | otherwise = False 