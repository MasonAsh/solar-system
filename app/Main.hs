module Main where

import Graphics.Gloss
import System.Random

_todo :: a
{-# WARNING
_todo "TODO"
 #-}

_todo = error "TODO"

type Vec2 = (Float, Float)

(<+>) :: Vec2 -> Vec2 -> Vec2
(<+>) (x0, y0) (x1, y1) = (x0 + x1, y0 + y1)

sunSize = 0.10

translateVec2 = uncurry Translate

data Orbit = Orbit
  { majorAxis :: Float
  , eccentricity :: Float
  , ptheta :: Float
  , t :: Float
  } deriving (Read)

eccentricAnomaly :: Float -> Float -> Float
eccentricAnomaly m e = last $ take 5 $ iterate ((+ m) . (* e) . sin) m

trueAnomaly e ea =
  2.0 * atan2 (sqrt (1.0 + e) * sin (ea / 2)) (sqrt (1.0 - e) * cos (ea / 2.0))

orbitPos orbit = (x, y)
  where
    x = r * cos (ta + w)
    y = r * sin (ta + w)
    r = a * ((1 - e ^ 2) / (1 + e * cos ta))
    ta = (trueAnomaly e ea)
    a = majorAxis orbit
    e = eccentricity orbit
    ea = eccentricAnomaly m e
    time = t orbit
    m = time * n
    n = (0.005 / a ^ 3)
    w = ptheta orbit

data Planet = Planet
  { orbit :: Orbit
  , radius :: Float
  , c :: Color
  , moons :: [Planet]
  }

data World =
  World [Planet]

unWorld (World planets) = planets

windowSize = (1280, 720)

windowSizeFlt =
  (fromIntegral $ fst windowSize, fromIntegral $ snd windowSize) :: ( Float
                                                                    , Float)

aspect = (fst windowSizeFlt / snd windowSizeFlt)

worldDim = (aspect, 1.0)

worldDimX = fst worldDim

worldDimY = snd worldDim

maxVel = 0.03

initialWorld =
  World
    [ Planet
        { orbit =
            Orbit
              {majorAxis = 0.7, eccentricity = 0.3, t = 0, ptheta = (pi * 0.3)}
        , radius = 0.02
        , c = white
        , moons =
            [ Planet
                { orbit =
                    Orbit
                      { majorAxis = 0.08
                      , eccentricity = 0.2
                      , t = 0
                      , ptheta = (pi * 0.3)
                      }
                , radius = 0.0015
                , c = white
                , moons = []
                }
            ]
        }
    , Planet
        { orbit =
            Orbit
              {majorAxis = 0.70, eccentricity = 0.24, t = -25, ptheta = pi * 1.3}
        , radius = 0.05
        , c = red
        , moons =
            [ Planet
                { orbit =
                    Orbit
                      { majorAxis = 0.12
                      , eccentricity = 0.05
                      , t = 0
                      , ptheta = (pi * 0.5)
                      }
                , radius = 0.016
                , c = magenta
                , moons = []
                }
            ]
        }
    , Planet
        { orbit =
            Orbit
              {majorAxis = 0.25, eccentricity = 0.08, t = 26, ptheta = pi * 3.0}
        , radius = 0.032
        , c = orange
        , moons = []
        }
    , Planet
        { orbit =
            Orbit
              {majorAxis = 0.90, eccentricity = 0.05, t = 20, ptheta = pi * 0.4}
        , radius = 0.032
        , c = blue
        , moons = []
        }
    ]

main :: IO ()
main = do
  let world = initialWorld
  play
    (InWindow "OrbitSim" windowSize (10, 10))
    black
    1000
    world
    render
    handleEvent
    updateWorld

worldSpaceToScreenSpace = Scale winScaler winScaler
  where
    winScaler = (minimum windowSizeFlt) / 2

thingRadius = 0.1

halfThingRadius = thingRadius / 2.0

sun :: Picture
sun = Color yellow $ circleSolid sunSize

renderPlanet :: Planet -> Picture
renderPlanet planet = Color (c planet) $ Pictures $ planetPic : moonPics
  where
    planetPic = translateVec2 planetPos $ circleSolid (radius planet)
    moonPics = map (translateVec2 planetPos . renderPlanet) (moons planet)
    planetPos = orbitPos . orbit $ planet

render (World planets) =
  worldSpaceToScreenSpace $ Pictures $ sun : map renderPlanet planets

handleEvent event world = world

updatePlanet dt planet@(Planet {orbit = o, moons = ms}) =
  planet {orbit = o {t = (t o) + dt}, moons = (map (updatePlanet dt) ms)}

updateWorld :: Float -> World -> World
updateWorld dt (World things) = World $ map (updatePlanet dt) things
