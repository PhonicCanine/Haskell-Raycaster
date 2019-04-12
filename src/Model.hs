module Model where

import CodeWorld
--                 Map    PLoc  Angle  Enemies   Projectiles
data Model = Model [Wall] Player [AIEnemy] [Projectile]

--                     Sprite  Loc.  Health Weapon
data AIEnemy = AIEnemy Picture Point Double Weapon

--                           Sprite  x,y   dx,dy Damage
data Projectile = Projectile Picture Point Point Double

--                   P. type    firer. ammo/c  t.ammo  auto
data Weapon = Weapon Projectile Double Integer Integer Bool

--                   loc.  angle. health weapon
data Player = Player Point Double Double Weapon

--                     List of coloured regions (pixels)
data Texture = Texture [ColouredRegion]

--                                   colour region
data ColouredRegion = ColouredRegion Colour Region

--                   tl    br
data Region = Region Point Point

--               tople brig  Type
data Wall = Wall Point Point Int
  deriving (Show)

-- | Starting Model for when CodeWorld first starts up.
initialModel :: Model
initialModel = Model initialMap (Player (0,0) 0 100 (Weapon (Projectile (coloured pink (solidCircle 0.5)) (0,0) (0,0) 1.0) 10 10 100 True)) [] []

initialMap :: [Wall]
initialMap = [(Wall ((-20),20) (20,10) 0), (Wall (10,10) (20,(-10)) 0), (Wall ((-20),10) ((-10),(-10)) 0), (Wall ((-20),(-20)) ((20),(-10)) 0), (Wall ((5),(5)) ((6),(6)) 2)]

builtInTextures :: [Texture]
builtInTextures = [checkeredTexture, plusTexture, orangeTexture]

plusTexture :: Texture
plusTexture = Texture [(ColouredRegion pink (Region (0,0) (0.25,0.25))), (ColouredRegion purple (Region (0,0.75) (0.25,1.0))), (ColouredRegion blue (Region (0.75,0) (1,0.25))), (ColouredRegion red (Region (0.75,0.75) (1,1))), (ColouredRegion black (Region (0,0) (1,1)))]

orangeTexture :: Texture
orangeTexture = Texture [(ColouredRegion red (Region (0,0) (1,1)))]

checkeredTexture :: Texture
checkeredTexture = Texture [(ColouredRegion cyan (Region (0,0) (0.5,0.5))),(ColouredRegion white (Region (0.5,0) (1,0.5))),(ColouredRegion white (Region (0,0.5) (0.5,1))),(ColouredRegion cyan (Region (0.5,0.5) (1,1)))]

defaultTexture :: Texture
defaultTexture = Texture [(ColouredRegion orange (Region (0,0) (1,1)))]

sky :: Colour
sky = blue

ground :: Colour
ground = brown