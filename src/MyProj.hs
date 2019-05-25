module MyProj
    ( runMyProj
    ) where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Data.ViewPort

type Point = (Int, Int)
type Checker = (MyProj.Point, Bool)

fps :: Int
fps = 60

cell :: Int
cell = 80

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
  , isEating :: Bool
  , mousePosition :: MyProj.Point
  , turn :: Bool
  , gameOver :: Int
  } deriving Show

initialState :: CheckersGame
initialState = Game 
  { myCheckers = initMyCheckers
  , enemyCheckers = initEnemyCheckers
  , active = (0, 0)
  , isActive = False
  , isKing = False
  , isEating = False
  , mousePosition = (0, 0)
  , turn = True
  , gameOver = 0
  }

initMyCheckers :: [MyProj.Checker]
initMyCheckers = (createPos (-280) (-280))
               ++ (createPos (-200) (-200))
               ++ (createPos (-280) (-120))

initEnemyCheckers :: [MyProj.Checker]
initEnemyCheckers = (createPos (-200) 280)
                  ++ (createPos (-280) 200)
                  ++ (createPos (-200) 120)

createPos :: Int -> Int -> [MyProj.Checker]
createPos x y | x <= 280 = ((x, y), False) : (createPos (x + 160) y)
              | otherwise = []

blackCells :: Int -> Int -> [Picture]
blackCells x y | x <= 280 && y <= 280 = [translate fx fy $ color (makeColor 0.5 0.25 0.125 1) $ rectangleSolid 80 80]
                                      ++ (blackCells (x + 80) (y + 80))
                                      ++ (blackCells (x + 160) y)
                                      ++ (blackCells x (y + 160))
               | otherwise = []
  where
    fx = fromIntegral x
    fy = fromIntegral y

gameToPicture :: [MyProj.Checker] -> Color -> [Picture]
gameToPicture [] _ = []
gameToPicture (x : xs) c | not (snd x) = [translate fx fy $ (color c) $ circleSolid 40] ++ (gameToPicture xs c)
                         | otherwise = [translate fx fy $ (color c) $ circleSolid 40]
                                    ++ [translate fx fy $ (color yellow) $ circleSolid 20] ++ (gameToPicture xs c)
  where
    fx = fromIntegral (fst (fst x))
    fy = fromIntegral (snd (fst x))

drawing :: CheckersGame -> Picture
drawing game | (gameOver game) == 1 = translate 0 0 $ (color red) $ rectangleSolid 640 640
             | (gameOver game) == 2 = translate 0 0 $ (color blue) $ rectangleSolid 640 640
             | (gameOver game) == 3 = translate 0 0 $ (color green) $ rectangleSolid 640 640
             | (isActive game) = pictures ((blackCells (-280) (-280))
             ++ [translate fx fy $ color green $ rectangleSolid 80 80]
             ++ (gameToPicture (myCheckers game) red)
             ++ gameToPicture (enemyCheckers game) blue)
             | otherwise = pictures ((blackCells (-280) (-280))
             ++ (gameToPicture (myCheckers game) red)
             ++ gameToPicture (enemyCheckers game) blue)
  where
    fx = fromIntegral (fst (active game))
    fy = fromIntegral (snd (active game))

tryActive :: MyProj.Point -> CheckersGame -> [MyProj.Checker] -> CheckersGame
tryActive _ game [] = game
tryActive z game (x : xs) = if dist <= 40
                              then game { isActive = True
                                        , isKing = snd x
                                        , active = fst x
                                        }
                              else tryActive z game xs
                            where dist = sqrt (fromIntegral ((fst z - fst (fst x))^2 + (snd z - snd (fst x))^2))

toInt :: Float -> Int
toInt = round

normalized :: MyProj.Point -> MyProj.Point
normalized z = (x, y)
  where
    x = fromIntegral ((fst z) `div` 80 * 80 + 40)
    y = fromIntegral ((snd z) `div` 80 * 80 + 40)

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

insideIsEmpty :: MyProj.Point -> MyProj.Point -> [MyProj.Checker] -> Bool
insideIsEmpty toOld from checkers | (fst to == fst from) || (snd to == snd from) = True
--                                  | (((fst to) + 40) `div` 80) /= 0 = True
--                                  | abs (fst to - fst from) > 320 = False
                                  | otherwise = (not (isMy to checkers))
                                             && insideIsEmpty to from checkers
  where to = sumPoints toOld (normalPoint (subPoints from toOld))

insideAmount :: MyProj.Point -> MyProj.Point -> [MyProj.Checker] -> Int
insideAmount toOld from checkers | (fst to == fst from) || (snd to == snd from) = 0
                                 | otherwise = if (isMy to checkers)
                                                 then 1 + insideAmount to from checkers
                                                 else insideAmount to from checkers
  where to = sumPoints toOld (normalPoint (subPoints from toOld))

--another one enemy under attack by one active
checkGoalFrom :: [MyProj.Checker] -> MyProj.Point -> CheckersGame -> Bool -> MyProj.Point -> Bool
checkGoalFrom [] _ _ _ _ = False
checkGoalFrom (to : xs) from game z active | z = abs (xTo - xFrom) == 80
                                              && abs (yTo - yFrom) == 80
                                              && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) from)) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (subPoints (fst to) from)) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (subPoints (fst to) from)))
                                              && ((absPoint (subPoints active from)) == (160, 160))
--                                              && (isEating game)
                                              || (isKing game)
                                              && abs (xTo - xFrom) == abs (yTo - yFrom)
                                              && insideIsEmpty (fst to) from ((myCheckers game) ++ (enemyCheckers game))
                                              && (not (isEnemy (sumPoints (fst to) (normalPoint (subPoints (fst to) from))) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (normalPoint (subPoints (fst to) from))) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (normalPoint (subPoints (fst to) from))))
                                              && insideAmount active from (enemyCheckers game) == 1
--                                              && (isEating game)
                                              || (checkGoalFrom xs from game z active)
                                           | otherwise = abs (xTo - xFrom) == 80
                                              && abs (yTo - yFrom) == 80
                                              && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) from)) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (subPoints (fst to) from)) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (subPoints (fst to) from)))
                                              && ((absPoint (subPoints active from)) == (160, 160))
--                                              && (isEating game)
                                              || (isKing game)
                                              && abs (xTo - xFrom) == abs (yTo - yFrom)
                                              && insideIsEmpty (fst to) from ((myCheckers game) ++ (enemyCheckers game))
                                              && (not (isEnemy (sumPoints (fst to) (normalPoint (subPoints (fst to) from))) (enemyCheckers game)))
                                              && (not (isMy (sumPoints (fst to) (normalPoint (subPoints (fst to) from))) (myCheckers game)))
                                              && (isIn (sumPoints (fst to) (normalPoint (subPoints (fst to) from))))
                                              && insideAmount (fst to) from (myCheckers game) == 1
--                                              && (isEating game)
                                              || (checkGoalFrom xs from game z active)
  where
    xTo = fst (fst to)
    yTo = snd (fst to)
    xFrom = fst from
    yFrom = snd from
--is enemies under attack by my
checkGoals :: [MyProj.Checker] -> [MyProj.Checker] -> CheckersGame -> Bool -> Bool
checkGoals [] _ _ _ = False
checkGoals _ [] _ _ = False
checkGoals (to : xs) (from : ys) game z | z = abs (xTo - xFrom) == 80
                                           && abs (yTo - yFrom) == 80
                                           && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) (fst from))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (subPoints (fst to) (fst from))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (subPoints (fst to) (fst from))))
                                           || (isKing game)
                                           && abs (xTo - xFrom) == abs (yTo - yFrom)
                                           && insideIsEmpty (fst to) (fst from) ((myCheckers game) ++ (enemyCheckers game))
                                           && (not (isEnemy (sumPoints (fst to) (normalPoint (subPoints (fst to) (fst from)))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (normalPoint (subPoints (fst to) (fst from)))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (normalPoint (subPoints (fst to) (fst from)))))
                                           || (checkGoals (to : xs) ys game z)
                                           || (checkGoals xs (from : ys) game z)
                                        | otherwise = abs (xTo - xFrom) == 80
                                           && abs (yTo - yFrom) == 80
                                           && (not (isEnemy (sumPoints (fst to) (subPoints (fst to) (fst from))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (subPoints (fst to) (fst from))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (subPoints (fst to) (fst from))))
                                           || (isKing game)
                                           && abs (xTo - xFrom) == abs (yTo - yFrom)
                                           && insideIsEmpty (fst to) (fst from) ((myCheckers game) ++ (enemyCheckers game))
                                           && (not (isEnemy (sumPoints (fst to) (normalPoint (subPoints (fst to) (fst from)))) (enemyCheckers game)))
                                           && (not (isMy (sumPoints (fst to) (normalPoint (subPoints (fst to) (fst from)))) (myCheckers game)))
                                           && (isIn (sumPoints (fst to) (normalPoint (subPoints (fst to) (fst from)))))
                                           || (checkGoals (to : xs) ys game z)
                                           || (checkGoals xs (from : ys) game z)
  where
    xTo = fst (fst to)
    yTo = snd (fst to)
    xFrom = fst (fst from)
    yFrom = snd (fst from)
--check moving or eating
checkGoal :: MyProj.Point -> MyProj.Point -> CheckersGame -> Bool
checkGoal to from game | (turn game) = abs (xTo - xFrom) == 80
                                    && (yTo - yFrom) == 80
                                    && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                    && (not (isMy (xTo, yTo) (myCheckers game)))
                                    && (not (checkGoals (enemyCheckers game) (myCheckers game) game (turn game)))
                                    && (isIn (xTo, yTo))
                                    || (isKing game)
                                    && abs (xTo - xFrom) == abs (yTo - yFrom)
                                    && insideIsEmpty to from ((myCheckers game) ++ (enemyCheckers game))
                                    && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                    && (not (isMy (xTo, yTo) (myCheckers game)))
--                                    && (not (checkGoals (enemyCheckers game) (myCheckers game) game (turn game)))
                                    && (isIn (xTo, yTo))
                                    || abs (xTo - xFrom) == 160
                                    && abs (yTo - yFrom) == 160
                                    && isEnemy (((xTo + xFrom) `div` 2), ((yTo + yFrom) `div` 2)) (enemyCheckers game)
                                    && (not (isMy (xTo, yTo) (myCheckers game)))
                                    && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                    && (isIn (xTo, yTo))
                                    || (isKing game)
                                    && abs (xTo - xFrom) == abs (yTo - yFrom)
                                    && abs (xTo - xFrom) /= 80
                                    && abs (yTo - yFrom) /= 80
                                    && insideAmount to from (enemyCheckers game) == 1
                                    && insideIsEmpty to from (myCheckers game)
                                    && (not (isMy (xTo, yTo) (myCheckers game)))
                                    && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                    && (isIn (xTo, yTo))
  where
    xTo = fst to
    yTo = snd to
    xFrom = fst from
    yFrom = snd from
checkGoal to from game | otherwise = abs (xTo - xFrom) == 80
                                  && (yTo - yFrom) == -80
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
                                  && (not (checkGoals (myCheckers game) (enemyCheckers game) game (turn game)))
                                  && (isIn (xTo, yTo))
--                                  && (not (isEating game))
                                  || (isKing game)
                                  && abs (xTo - xFrom) == abs (yTo - yFrom)
                                  && insideIsEmpty to from ((myCheckers game) ++ (enemyCheckers game))
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
--                                  && (not (checkGoals (myCheckers game) (enemyCheckers game) game (turn game)))
                                  && (isIn (xTo, yTo))
--                                  && (not (isEating game))
                                  || abs (xTo - xFrom) == 160
                                  && abs (yTo - yFrom) == 160
                                  && isMy (((xTo + xFrom) `div` 2), ((yTo + yFrom) `div` 2)) (myCheckers game)
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (isIn (xTo, yTo))
                                  || (isKing game)
                                  && abs (xTo - xFrom) == abs (yTo - yFrom)
                                  && abs (xTo - xFrom) /= 80
                                  && abs (yTo - yFrom) /= 80
                                  && insideAmount to from (myCheckers game) == 1
                                  && insideIsEmpty to from (enemyCheckers game)
--                                  && insideIsEmpty to from ((myCheckers game) ++ (enemyCheckers game))
--                                  && isMy (((xTo + xFrom) `div` 2), ((yTo + yFrom) `div` 2)) (enemyCheckers game)
                                  && (not (isMy (xTo, yTo) (myCheckers game)))
                                  && (not (isEnemy (xTo, yTo) (enemyCheckers game)))
                                  && (isIn (xTo, yTo))
  where
    xTo = fst to
    yTo = snd to
    xFrom = fst from
    yFrom = snd from

toIntPoint :: (Float, Float) -> MyProj.Point
toIntPoint x = (toInt (fst x), toInt (snd x))

normalPoint :: MyProj.Point -> MyProj.Point
normalPoint x | fst x >= 0 && snd x >= 0 = (80, 80)
              | fst x >= 0 && snd x < 0 = (80, -80)
              | fst x < 0 && snd x >= 0 = (-80, 80)
              | fst x < 0 && snd x < 0 = (-80, -80)
--normalPoint x = mulPoints (divPoints (x) (fst (absPoint x))) 80

absPoint :: MyProj.Point -> MyProj.Point
absPoint x = (abs (fst x), abs (snd x))

sumPoints :: MyProj.Point -> MyProj.Point -> MyProj.Point
sumPoints x y = ((fst x) + (fst y), (snd x) + (snd y))

subPoints :: MyProj.Point -> MyProj.Point -> MyProj.Point
subPoints x y = ((fst x) - (fst y), (snd x) - (snd y))

mulPoints :: MyProj.Point -> Int -> MyProj.Point
mulPoints x y = ((fst x) * y, (snd x) * y)

divPoints :: MyProj.Point -> Int -> MyProj.Point
divPoints x y = ((fst x) `div` y, (snd x) `div` y)

--mp - mousePosition
deleteOld :: MyProj.Point -> MyProj.Point -> [MyProj.Checker] -> [MyProj.Checker]
deleteOld _ _ [] = []
deleteOld active mp (x : xs) = (deleteInside active mp x) ++ deleteOld active mp xs

deleteInside :: MyProj.Point -> MyProj.Point -> MyProj.Checker -> [MyProj.Checker]
deleteInside active mp x | active == (fst x) = []
                         | active == mp = [x]
                         | otherwise = deleteInside (sumPoints active (normalPoint (subPoints mp active))) mp x

checkIsEating :: MyProj.Point -> MyProj.Point -> [MyProj.Checker] -> Bool
checkIsEating _ _ [] = False
checkIsEating active mp (x : xs) = (checkInside active mp x)
                                 || checkIsEating active mp xs

checkInside :: MyProj.Point -> MyProj.Point -> MyProj.Checker -> Bool
checkInside active mp x | active == (fst x) = True
                        | active == mp = False
                        | otherwise = checkInside (sumPoints active (normalPoint (subPoints mp active))) mp x

setKing :: [MyProj.Checker] -> Bool -> [MyProj.Checker]
setKing [] _ = []
setKing (x : xs) turn | turn = if snd (fst x) == 280
                                 then (fst x, True) : setKing xs turn
                                 else x : setKing xs turn
                      | otherwise = if snd (fst x) == -280
                                 then (fst x, True) : setKing xs turn
                                 else x : setKing xs turn

checkForWin :: CheckersGame -> CheckersGame
checkForWin game | (myCheckers game) == [] = game { gameOver = 2 }
                 | (enemyCheckers game) == [] = game { gameOver = 1 }
                 | otherwise = game

keys :: Event -> CheckersGame -> CheckersGame
keys (EventKey (MouseButton LeftButton) Down _ _) game
--1 player move
  | (isActive game) == True = if (checkGoal (normalized (mousePosition game)) (active game) game) && (turn game)
    then checkForWin game
      { myCheckers = setKing (deleteOld (active game) (active game) ((myCheckers game) ++ [(normalized (mousePosition game), isKing game)])) (turn game)
      , enemyCheckers = deleteOld (active game) (normalized (mousePosition game)) (enemyCheckers game)
--      , isEating = checkIsEating (active game) (normalized (mousePosition game)) (enemyCheckers game)
--      && checkGoalFrom (enemyCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      , isActive = checkGoalFrom (enemyCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      , turn = checkGoalFrom (enemyCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      , active = normalized (mousePosition game)
      }
--2 player move
  else if checkGoal (normalized(mousePosition game)) (active game) game
    then checkForWin game
      { myCheckers = deleteOld (active game) (normalized (mousePosition game)) (myCheckers game)
      , enemyCheckers = setKing (deleteOld (active game) (active game) ((enemyCheckers game) ++ [(normalized (mousePosition game), isKing game)])) (turn game)
--      , isEating = checkIsEating (active game) (normalized (mousePosition game)) (myCheckers game)
--      && checkGoalFrom (myCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      , isActive = checkGoalFrom (myCheckers game) (normalized (mousePosition game)) game (turn game) (active game)
      , turn = not (checkGoalFrom (myCheckers game) (normalized (mousePosition game)) game (turn game) (active game))
      , active = normalized (mousePosition game)
      }
  else game {isActive = False}
  | otherwise = if (turn game)
    then tryActive (mousePosition game) game (myCheckers game)
    else tryActive (mousePosition game) game (enemyCheckers game)
keys (EventMotion x) game = game {mousePosition = toIntPoint x}
keys _ game = game

update :: Float -> CheckersGame -> CheckersGame
update _ game = game

runMyProj :: IO ()
runMyProj = play window background fps initialState drawing keys update
