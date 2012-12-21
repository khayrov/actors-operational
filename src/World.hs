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
import Debug.Trace

import Player hiding (time)


data PlayerState = PlayerState Player (Behavior ())


instance Show PlayerState where
    show (PlayerState p _) = "PlayerState (" ++ show p ++ ")"


data World = World {
    time :: Float,
    bounds :: (Vec2, Vec2),
    players :: Map PlayerID PlayerState,
    rng :: StdGen
} deriving (Show)


mkWorld :: [Player] -> IO World
mkWorld players = do
    rng <- newStdGen
    let pmap = Map.fromList [(playerId p, PlayerState p (behavior p)) | p <- players]
    return $ World 0 (Vec2 0 0, Vec2 200 200) pmap rng


worldPlayers :: World -> [Player]
worldPlayers world = [p | PlayerState p _ <- Map.elems (players world)]


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
            Just (PlayerState player behav) -> do
                let (prog, mem') = runState (viewT behav) (memory player)
                let player' = player { memory = mem' }
                state' <- eval player' prog
                modify $ \w -> w { players = Map.insert pid state' players }
            Nothing -> return ()
    where
    eval :: Player -> BehaviorView () -> State World PlayerState
    eval player (GetEnv :>>= cont) = do
        world@(World { .. }) <- get
        let others = [p | p <- worldPlayers world, playerId p /= pid]
        let env = Environment time player others
        return $ PlayerState player (cont env)
    eval player (GetRandomR range :>>= cont) = do
        g <- gets rng
        let (x, g') = randomR range g
        modify $ \w -> w { rng = g' }
        return $ PlayerState player (cont x)
    eval player (Move v :>>= cont) = do
        wbounds <- gets bounds
        let pos' = fitBounds wbounds $ position player &+ v &* timestep
        let p' = player { position = pos', velocity = v }
        return $ PlayerState p' (cont pos')
    eval player (Return _) =
        return $ PlayerState player (behavior player)
