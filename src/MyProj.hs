module MyProj
    ( runMyProj
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

type Point = (Float, Float)
type Checker = (MyProj.Point, Bool)

fps :: Int
fps = 60

window :: Display
window = InWindow "Nice Window" (640, 640) (100, 100)

background :: Color
background = white

data CheckersGame = Game
  { myCheckers :: [MyProj.Checker]
  , enemyCheckers :: [MyProj.Checker]
  , active :: MyProj.Point
  , isActive :: Bool
  , isKing :: Bool
  , mousePosition :: MyProj.Point
  , turn :: Bool
  } deriving Show

initialState :: CheckersGame
initialState = Game 
  { myCheckers = initMyCheckers
  , enemyCheckers = initEnemyCheckers
  , active = (0, 0)
  , isActive = False
  , isKing = False
  , mousePosition = (0, 0)
  , turn = True
  }

initMyCheckers :: [MyProj.Checker]
initMyCheckers = (createPos (-280) (-280))
               ++ (createPos (-200) (-200))
               ++ (createPos (-280) (-120))

initEnemyCheckers :: [MyProj.Checker]
initEnemyCheckers = (createPos (-200) 280)
                  ++ (createPos (-280) 200)
                  ++ (createPos (-200) 120)

createPos :: Float -> Float -> [MyProj.Checker]
createPos x y | x <= 280 = ((x, y), False) : (createPos (x + 160) y)
              | otherwise = []

blackCells :: Float -> Float -> [Picture]
blackCells x y | x <= 280 && y <= 280 = [translate x y $ color (makeColor 0.5 0.25 0.125 1) $ rectangleSolid 80 80]
                                      ++ (blackCells (x + 80) (y + 80))
                                      ++ (blackCells (x + 160) y)
                                      ++ (blackCells x (y + 160))
               | otherwise = []

gameToPicture :: [MyProj.Checker] -> Color -> [Picture]
gameToPicture [] _ = []
gameToPicture (x : xs) c = [translate (fst (fst x)) (snd (fst x)) $ (color c) $ circleSolid 40] ++ (gameToPicture xs c)

drawing :: CheckersGame -> Picture
drawing game | (isActive game) = pictures ((blackCells (-280) (-280))
             ++ [translate (fst (active game)) (snd (active game)) $ color green $ rectangleSolid 80 80]
             ++ (gameToPicture (myCheckers game) red)
             ++ gameToPicture (enemyCheckers game) blue)
             | otherwise = pictures ((blackCells (-280) (-280))
             ++ (gameToPicture (myCheckers game) red)
             ++ gameToPicture (enemyCheckers game) blue)

tryActive :: MyProj.Point -> CheckersGame -> [MyProj.Checker] -> CheckersGame
tryActive _ game [] = game
tryActive z game (x : xs) = if dist <= 40
                              then game { isActive = True
                                        , isKing = snd x
                                        , active = fst x
                                        }
                              else tryActive z game xs
                            where dist = sqrt ((fst z - fst (fst x))^2 + (snd z - snd (fst x))^2)

toInt :: Float -> Int
toInt = round

normalized :: MyProj.Point -> MyProj.Point
normalized z = (x, y)
  where
    x = fromIntegral ((toInt (fst z)) `div` 80 * 80 + 40)
    y = fromIntegral ((toInt (snd z)) `div` 80 * 80 + 40)

isEnemy :: MyProj.Point -> [MyProj.Checker] -> Bool
isEnemy _ [] = False
isEnemy (x, y) (z : zs) = x == fst (fst z) && y == snd (fst z) || isEnemy (x, y) zs

isMy :: MyProj.Point -> [MyProj.Checker] -> Bool
isMy _ [] = False
isMy (x, y) (z : zs) = x == fst (fst z) && y == snd (fst z) || isMy (x, y) zs

isIn :: MyProj.Point -> Bool
isIn (x, y) = x >= -280
           && x <= 280
           && y >= -280
           && y <= 280
--another one enemies under attack by one active
checkGoalFrom :: [MyProj.Checker] -> MyProj.Point -> CheckersGame -> Bool -> MyProj.Point -> Bool
checkGoalFrom [] _ _ _ _ = False
checkGoalFrom (to : xs) from game z active | z = abs (xTo - xFrom) == 80
                                              && (yTo - yFrom) == 80
                                              && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) from)) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (subPoints (fst to) from)) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (subPoints (fst to) from)))
                                              && ((absPoint (subPoints active from)) == (160, 160))
                                              || (isKing game)
                                              && abs (xTo - xFrom) == 80
                                              && (yTo - yFrom) == -80
                                              && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) from)) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (subPoints (fst to) from)) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (subPoints (fst to) from)))
                                              && ((absPoint (subPoints active from)) == (160, 160))
                                              || (checkGoalFrom xs from game z active)
                                           | otherwise = abs (xTo - xFrom) == 80
                                              && (yTo - yFrom) == -80
                                              && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) from)) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (subPoints (fst to) from)) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (subPoints (fst to) from)))
                                              && ((absPoint (subPoints active from)) == (160, 160))
                                              || (isKing game)
                                              && abs (xTo - xFrom) == 80
                                              && (yTo - yFrom) == 80
                                              && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) from)) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (subPoints (fst to) from)) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (subPoints (fst to) from)))
                                              && ((absPoint (subPoints active from)) == (160, 160))
                                              || (checkGoalFrom xs from game z active)
  where
    xTo = fromIntegral ((toInt (fst (fst to))) `div` 80 * 80 + 40)
    yTo = fromIntegral ((toInt (snd (fst to))) `div` 80 * 80 + 40)
    xFrom = fst from
    yFrom = snd from
--is enemies under attack by my
checkGoals :: [MyProj.Checker] -> [MyProj.Checker] -> CheckersGame -> Bool -> Bool
checkGoals [] _ _ _ = False
checkGoals _ [] _ _ = False
checkGoals (to : xs) (from : ys) game z | z = abs (xTo - xFrom) == 80
                                           && (yTo - yFrom) == 80
                                           && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) (fst from))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (subPoints (fst to) (fst from))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (subPoints (fst to) (fst from))))
                                           || (isKing game)
                                           && abs (xTo - xFrom) == 80
                                           && (yTo - yFrom) == -80
                                           && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) (fst from))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (subPoints (fst to) (fst from))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (subPoints (fst to) (fst from))))
                                           || (checkGoals (to : xs) ys game z)
                                           || (checkGoals xs (from : ys) game z)
                                        | otherwise = abs (xTo - xFrom) == 80
                                           && (yTo - yFrom) == -80
                                           && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) (fst from))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (subPoints (fst to) (fst from))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (subPoints (fst to) (fst from))))
                                           || (isKing game)
                                           && abs (xTo - xFrom) == 80
                                           && (yTo - yFrom) == 80
                                           && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) (fst from))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (subPoints (fst to) (fst from))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (subPoints (fst to) (fst from))))
                                           || (checkGoals (to : xs) ys game z)
                                           || (checkGoals xs (from : ys) game z)
  where
    xTo = fromIntegral ((toInt (fst (fst to))) `div` 80 * 80 + 40)
    yTo = fromIntegral ((toInt (snd (fst to))) `div` 80 * 80 + 40)
    xFrom = fst (fst from)
    yFrom = snd (fst from)
--is one enemy under attack by one
checkGoal :: MyProj.Point -> MyProj.Point -> CheckersGame -> Bool
checkGoal to from game | (turn game) == True = abs (xTo - xFrom) == 80
                                            && (yTo - yFrom) == 80
                                            && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                            && (not (isMy (xTo, yTo) (myCheckers game)))
                                            && (not (checkGoals (enemyCheckers game) (myCheckers game) game (turn game)))
                                            && (isIn (xTo, yTo))
                                            || (isKing game)
                                            && abs (xTo - xFrom) == 80
                                            && (yTo - yFrom) == -80
                                            && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                            && (not (isMy (xTo, yTo) (myCheckers game)))
                                            && (not (checkGoals (enemyCheckers game) (myCheckers game) game (turn game)))
                                            && (isIn (xTo, yTo))
                                            || abs (xTo - xFrom) == 160
                                            && (yTo - yFrom) == 160
                                            && isEnemy (((xTo + xFrom) / 2), ((yTo + yFrom) / 2)) (enemyCheckers game)
                                            && (not (isMy (xTo, yTo) (myCheckers game)))
                                            && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                            && (isIn (xTo, yTo))
                                            || (isKing game)
                                            && abs (xTo - xFrom) == 160
                                            && (yTo - yFrom) == -160
                                            && isEnemy (((xTo + xFrom) / 2), ((yTo + yFrom) / 2)) (enemyCheckers game)
                                            && (not (isMy (xTo, yTo) (myCheckers game)))
                                            && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                            && (isIn (xTo, yTo))
  where
    xTo = fromIntegral ((toInt (fst to)) `div` 80 * 80 + 40)
    yTo = fromIntegral ((toInt (snd to)) `div` 80 * 80 + 40)
    xFrom = fst from
    yFrom = snd from
checkGoal to from game | otherwise = abs (xTo - xFrom) == 80
                                  && (yTo - yFrom) == -80
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
                                  && (not (checkGoals (myCheckers game) (enemyCheckers game) game (turn game)))
                                  && (isIn (xTo, yTo))
                                  || (isKing game)
                                  && abs (xTo - xFrom) == 80
                                  && (yTo - yFrom) == 80
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
                                  && (not (checkGoals (enemyCheckers game) (myCheckers game) game (turn game)))
                                  && (isIn (xTo, yTo))
                                  || abs (xTo - xFrom) == 160
                                  && (yTo - yFrom) == -160
                                  && isMy (((xTo + xFrom) / 2), ((yTo + yFrom) / 2)) (myCheckers game)
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (isIn (xTo, yTo))
                                  || (isKing game)
                                  && abs (xTo - xFrom) == 160
                                  && (yTo - yFrom) == 160
                                  && isEnemy (((xTo + xFrom) / 2), ((yTo + yFrom) / 2)) (enemyCheckers game)
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (isIn (xTo, yTo))
  where
    xTo = fromIntegral ((toInt (fst to)) `div` 80 * 80 + 40)
    yTo = fromIntegral ((toInt (snd to)) `div` 80 * 80 + 40)
    xFrom = fst from
    yFrom = snd from

deleteOld :: MyProj.Point -> [MyProj.Checker] -> [MyProj.Checker]
deleteOld _ [] = []
deleteOld y (x : xs) | (fst x) == y = xs
                     | otherwise = x : deleteOld y xs

setKing :: [MyProj.Checker] -> Bool -> [MyProj.Checker]
setKing [] _ = []
setKing (x : xs) turn | turn = if snd (fst x) == 280
                                 then (fst x, True) : setKing xs turn
                                 else x : setKing xs turn
                      | otherwise = if snd (fst x) == -280
                                 then (fst x, True) : setKing xs turn
                                 else x : setKing xs turn

absPoint :: MyProj.Point -> MyProj.Point
absPoint x = (abs (fst x), abs (snd x))

sumPoints :: MyProj.Point -> MyProj.Point -> MyProj.Point
sumPoints x y = ((fst x) + (fst y), (snd x) + (snd y))

subPoints :: MyProj.Point -> MyProj.Point -> MyProj.Point
subPoints x y = ((fst x) - (fst y), (snd x) - (snd y))

divPoints :: MyProj.Point -> Float -> MyProj.Point
divPoints x y = ((fst x) / y, (snd x) / y)

keys :: Event -> CheckersGame -> CheckersGame
keys (EventKey (MouseButton LeftButton) Down _ _) game
--1 player move
  | (isActive game) == True = if (checkGoal (mousePosition game) (active game) game) && (turn game)
    then game
      { myCheckers = setKing (deleteOld (active game) (myCheckers game) ++ [(normalized (mousePosition game), isKing game)]) (turn game)
      , enemyCheckers = setKing (deleteOld (divPoints (sumPoints (active game) (normalized (mousePosition game))) 2) (enemyCheckers game)) (turn game)
      , isActive = checkGoalFrom (enemyCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      , active = normalized (mousePosition game)
      , turn = checkGoalFrom (enemyCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      }
--2 player move
  else if checkGoal (mousePosition game) (active game) game
    then game
      { myCheckers = setKing (deleteOld (divPoints (sumPoints (active game) (normalized (mousePosition game))) 2) (myCheckers game)) (turn game)
      , enemyCheckers = setKing (deleteOld (active game) (enemyCheckers game) ++ [(normalized (mousePosition game), isKing game)]) (turn game)
      , isActive = checkGoalFrom (myCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      , active = normalized (mousePosition game)
      , turn = not (checkGoalFrom (myCheckers game) (normalized (mousePosition game)) game (turn game) (active game))
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
