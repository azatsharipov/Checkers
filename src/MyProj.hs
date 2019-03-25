module MyProj
    ( runMyProj
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

type Point = (Float, Float)

fps :: Int
fps = 60

window :: Display
window = InWindow "Nice Window" (640, 640) (100, 100)

background :: Color
background = white

data CheckersGame = Game
  { myCheckers :: [MyProj.Point]
  , enemyCheckers :: [MyProj.Point]
  , active :: MyProj.Point
  , isActive :: Bool
  , mousePosition :: MyProj.Point
  , turn :: Bool
  } deriving Show

initialState :: CheckersGame
initialState = Game 
  { myCheckers = initMyCheckers
  , enemyCheckers = initEnemyCheckers
  , active = (0, 0)
  , isActive = False
  , mousePosition = (0, 0)
  , turn = True
  }

initMyCheckers :: [MyProj.Point]
initMyCheckers = (createPos (-280) (-280))
               ++ (createPos (-200) (-200))
               ++ (createPos (-280) (-120))

initEnemyCheckers :: [MyProj.Point]
initEnemyCheckers = (createPos (-200) 280)
                  ++ (createPos (-280) 200)
                  ++ (createPos (-200) 120)

createPos :: Float -> Float -> [MyProj.Point]
createPos x y | x <= 280 = (x, y) : (createPos (x + 160) y)
              | otherwise = []

blackCells :: Float -> Float -> [Picture]
blackCells x y | x <= 280 && y <= 280 = [translate x y $ color (makeColor 0.5 0.25 0.125 1) $ rectangleSolid 80 80]
                                      ++ (blackCells (x + 80) (y + 80))
                                      ++ (blackCells (x + 160) y)
                                      ++ (blackCells x (y + 160))
               | otherwise = []

gameToPicture :: [MyProj.Point] -> Color -> [Picture]
gameToPicture [] _ = []
gameToPicture (x : xs) c = [translate (fst x) (snd x) $ (color c) $ circleSolid 40] ++ (gameToPicture xs c)

drawing :: CheckersGame -> Picture
drawing game | (isActive game) = pictures ((blackCells (-280) (-280))
             ++ [translate (fst (active game)) (snd (active game)) $ color green $ rectangleSolid 80 80]
             ++ (gameToPicture (myCheckers game) red)
             ++ gameToPicture (enemyCheckers game) blue)
             | otherwise = pictures ((blackCells (-280) (-280))
             ++ (gameToPicture (myCheckers game) red)
             ++ gameToPicture (enemyCheckers game) blue)

tryActive :: MyProj.Point -> CheckersGame -> [MyProj.Point] -> CheckersGame
tryActive _ game [] = game
tryActive z game (x : xs) = if dist <= 40
                              then game { isActive = True
                                        , active = x
                                        }
                              else tryActive z game xs
                            where dist = sqrt ((fst z - fst x)^2 + (snd z - snd x)^2)

toInt :: Float -> Int
toInt = round

normalized :: MyProj.Point -> MyProj.Point
normalized z = (x, y)
  where
    x = fromIntegral ((toInt (fst z)) `div` 80 * 80 + 40)
    y = fromIntegral ((toInt (snd z)) `div` 80 * 80 + 40)

isEnemy :: Float -> Float -> [MyProj.Point] -> Bool
isEnemy _ _ [] = False
isEnemy x y (z : zs) = x == fst z && y == snd z || isEnemy x y zs

isMy :: Float -> Float -> [MyProj.Point] -> Bool
isMy _ _ [] = False
isMy x y (z : zs) = x == fst z && y == snd z || isMy x y zs

checkGoal :: MyProj.Point -> MyProj.Point -> CheckersGame -> Bool
checkGoal to from game | (turn game) == True = abs (xTo - xFrom) == 80
                                            && (yTo - yFrom) == 80
                                            && (not (isEnemy xTo yTo (enemyCheckers game)))
                                            && (not (isMy xTo yTo (myCheckers game)))
                                            || abs (xTo - xFrom) == 160
                                            && (yTo - yFrom) == 160
                                            && isEnemy ((xTo + xFrom) / 2) ((yTo + yFrom) / 2) (enemyCheckers game)
                                            && (not (isMy xTo yTo (myCheckers game)))
                                            && (not (isEnemy xTo yTo (enemyCheckers game)))
  where
    xTo = fromIntegral ((toInt (fst to)) `div` 80 * 80 + 40)
    yTo = fromIntegral ((toInt (snd to)) `div` 80 * 80 + 40)
    xFrom = fst from
    yFrom = snd from
checkGoal to from game | otherwise = abs (xTo - xFrom) == 80
                                  && (yTo - yFrom) == -80
                                  && (not (isEnemy xTo yTo (enemyCheckers game)))
                                  && (not (isMy xTo yTo (myCheckers game)))
                                  || abs (xTo - xFrom) == 160
                                  && (yTo - yFrom) == -160
                                  && isMy ((xTo + xFrom) / 2) ((yTo + yFrom) / 2) (myCheckers game)
                                  && (not (isMy xTo yTo (myCheckers game)))
                                  && (not (isEnemy xTo yTo (enemyCheckers game)))
  where
    xTo = fromIntegral ((toInt (fst to)) `div` 80 * 80 + 40)
    yTo = fromIntegral ((toInt (snd to)) `div` 80 * 80 + 40)
    xFrom = fst from
    yFrom = snd from

deleteOld :: MyProj.Point -> [MyProj.Point] -> [MyProj.Point]
deleteOld _ [] = []
deleteOld y (x : xs) | x == y = xs
                     | otherwise = x : deleteOld y xs

sumPoints :: MyProj.Point -> MyProj.Point -> MyProj.Point
sumPoints x y = ((fst x) + (fst y), (snd x) + (snd y))

dividePoints :: MyProj.Point -> Float -> MyProj.Point
dividePoints x y = ((fst x) / y, (snd x) / y)

keys :: Event -> CheckersGame -> CheckersGame
keys (EventKey (MouseButton LeftButton) Down _ _) game
--1 player move
  | (isActive game) == True = if (checkGoal (mousePosition game) (active game) game) && (turn game)
    then game
      { myCheckers = deleteOld (active game) (myCheckers game) ++ [normalized (mousePosition game)]
      , enemyCheckers = deleteOld (dividePoints (sumPoints (active game) (normalized (mousePosition game))) 2) (enemyCheckers game)
      , isActive = False
      , turn = (not (turn game))
      }
--2 player move
  else if checkGoal (mousePosition game) (active game) game
    then game
      { myCheckers = deleteOld (dividePoints (sumPoints (active game) (normalized (mousePosition game))) 2) (myCheckers game)
      , enemyCheckers = deleteOld (active game) (enemyCheckers game) ++ [normalized (mousePosition game)]
      , isActive = False
      , turn = (not (turn game))
      }
  else game {isActive = False}
  | otherwise = if (turn game)
    then tryActive (mousePosition game) game (myCheckers game)
    else tryActive (mousePosition game) game (enemyCheckers game)
keys (EventMotion x) game = game {mousePosition = x}
keys _ game = game

update :: Float -> CheckersGame -> CheckersGame
update _ game = game

runMyProj :: IO ()
runMyProj = play window background fps initialState drawing keys update
