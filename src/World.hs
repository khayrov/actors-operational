module World (
    World(..),
    mkWorld,
    worldPlayers,
    stepWorld
) where


import Control.Monad.Operational
import Control.Monad.State
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import Data.Vect.Float
import System.Random

import Player hiding (time)


data World = World {
    time :: Float,
    bounds :: (Vec2, Vec2),
    players :: Map PlayerID Player,
    rng :: StdGen
} deriving (Show)


mkWorld :: [Player] -> IO World
mkWorld players = do
    rng <- newStdGen
    let pmap = Map.fromList [(playerId p, p) | p <- players]
    return $ World 0 (Vec2 0 0, Vec2 200 200) pmap rng


worldPlayers :: World -> [Player]
worldPlayers = Map.elems . players


fitBounds :: (Vec2, Vec2) -> Vec2 -> Vec2
fitBounds (Vec2 x0 y0, Vec2 x1 y1) (Vec2 x y) =
    Vec2 (clamp x x0 x1) (clamp y y0 y1)
    where
    clamp x a b = max a (min x b)


stepWorld :: Float -> World -> World
stepWorld timestep world@(World { .. }) = world' { time = time + timestep } where
    world' = execState (mapM_ (runPlayer timestep) (Map.keys players)) world


runPlayer :: Float -> PlayerID -> State World ()
runPlayer timestep pid = do
    World { .. } <- get
    case Map.lookup pid players of
            Just player@(Player { .. }) -> do
                player' <- eval player (runBrain brain)
                modify $ \w -> w { players = Map.insert pid player' players }
            Nothing -> return ()
    where
    eval :: Player -> BrainCont -> State World Player
    eval player (BrainCont m def (GetEnv :>>= cont)) = do
        world@(World { .. }) <- get
        let others = [p | p <- worldPlayers world, playerId p /= pid]
        let env = Environment time player others
        return $ player { brain = Brain m def (cont env) }
    eval player (BrainCont m def (GetRandomR range :>>= cont)) = do
        g <- gets rng
        let (x, g') = randomR range g
        modify $ \w -> w { rng = g' }
        return $ player { brain = Brain m def (cont x) }
    eval player (BrainCont m def (Move v :>>= cont)) = do
        wbounds <- gets bounds
        let pos = position player
        let pos' = fitBounds wbounds $ position player &+ v &* timestep
        let vel' = (pos' &- pos) &* (1 / timestep)
        let player' = player { position = pos', velocity = vel' }
        return $ player' { brain = Brain m def (cont pos') }
    eval player (BrainCont m def (Return _)) =
        return $ player { brain = Brain m def def }
