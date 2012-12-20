module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import qualified Data.Map.Strict as Map
import Data.Vect.Float
import Graphics.Gloss

import Player
import World hiding (time)


repeatFor :: Float -> Behavior a -> Behavior ()
repeatFor duration action = do
    t0 <- askenv time
    let t1 = t0 + duration
    whileM_ ((< t1) <$> askenv time) action


sleep :: Float -> Behavior ()
sleep delay = repeatFor delay idle 


randomRunner :: Behavior ()
randomRunner = do
    v <- Vec2 <$> getRandomR (-20, 20) <*> getRandomR (-20, 20)
    t <- getRandomR (1, 5)
    repeatFor t $ move v
    t <- getRandomR (0, 5)
    sleep t


renderWorld :: World -> Picture
renderWorld world =
    pictures $ map renderPlayer $ worldPlayers world
    where
    renderPlayer (Player { position = (Vec2 x y) }) =
        translate x y $ color black $ circleSolid 3


main :: IO ()
main = do
    let p1 = Player (PlayerID 1) (Vec2 50 50) zero randomRunner
    let p2 = Player (PlayerID 2) (Vec2 100 100) zero randomRunner
    world <- mkWorld [p1, p2]
    simulate
        (InWindow "operational" (300, 300) (100, 100))
        white
        60
        world
        renderWorld
        (const stepWorld)
