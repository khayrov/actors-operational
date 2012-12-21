module Main (main) where

import Control.Applicative
import Control.Monad
import Control.Monad.Loops
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Vect.Float
import Graphics.Gloss

import Player
import World hiding (time)


data Memory = Memory {
    counter :: Int
} deriving (Show)


type AI = Behavior Memory


repeatFor :: Float -> Behavior m a -> Behavior m ()
repeatFor duration action = do
    t0 <- askenv time
    let t1 = t0 + duration
    whileM_ ((< t1) <$> askenv time) action


sleep :: Float -> Behavior m ()
sleep delay = repeatFor delay idle 


randomRunner :: AI ()
randomRunner = do
    modify $ \m -> m { counter = counter m + 1 }
    c <- gets counter
    replicateM_ c $ do
        v <- Vec2 <$> getRandomR (-30, 30) <*> getRandomR (-30, 30)
        t <- getRandomR (1, 5)
        repeatFor t $ move v
    t <- getRandomR (0, 5)
    sleep t


renderWorld :: World -> Picture
renderWorld world =
    color black $ pictures $ frame : (map renderPlayer $ worldPlayers world)
    where
    renderPlayer (Player { position = (Vec2 x y) }) =
        translate x y $ color black $ circleSolid 3
    frame = lineLoop [(0, 0), (0, 200), (200, 200), (200, 0)]


main :: IO ()
main = do
    let b1 = Brain (Memory 0) randomRunner randomRunner
    let b2 = Brain (Memory 0) randomRunner randomRunner
    let p1 = Player (PlayerID 1) (Vec2 50 50) zero b1
    let p2 = Player (PlayerID 2) (Vec2 100 100) zero b2
    world <- mkWorld [p1, p2]
    simulate
        (InWindow "operational" (300, 300) (100, 100))
        white
        60
        world
        renderWorld
        (const stepWorld)
