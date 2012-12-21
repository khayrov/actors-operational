module Player (
    PlayerID(..),
    Player(..), Memory(..),
    Environment(..),
    PlayerAction(..), getenv, move, getRandomR, idle, askenv,
    Behavior, BehaviorView
) where


import Control.Monad (void)
import Control.Monad.Operational
import Control.Monad.State
import Data.List (intercalate)
import Data.Vect.Float (Vec2, zero)
import System.Random (Random)


newtype PlayerID = PlayerID Int deriving (Show, Eq, Ord)


data Player = Player {
    playerId :: PlayerID,
    position :: Vec2,
    velocity :: Vec2,
    memory :: Memory,
    behavior :: Behavior ()
}


instance Show Player where
    show (Player { .. }) = "Player { " ++ intercalate ", " [
            show playerId,
            "position = " ++ show position,
            "velocity = " ++ show velocity ]
        ++ " }"


instance Eq Player where
    p1 == p2 = playerId p1 == playerId p2


instance Ord Player where
    compare p1 p2 = compare (playerId p1) (playerId p2)


data Environment = Environment {
    time :: Float,
    self :: Player,
    others :: [Player]
} deriving (Show)


data PlayerAction a where
    GetEnv :: PlayerAction Environment
    Move :: Vec2 -> PlayerAction Vec2
    GetRandomR :: Random a => (a, a) -> PlayerAction a


data Memory = Memory {
    counter :: Int
} deriving (Show)


type Behavior = ProgramT PlayerAction (State Memory)

type BehaviorView = ProgramViewT PlayerAction (State Memory)


getenv :: Behavior Environment
getenv = singleton GetEnv

move :: Vec2 -> Behavior Vec2
move = singleton . Move

getRandomR :: Random a => (a, a) -> Behavior a
getRandomR = singleton . GetRandomR

idle :: Behavior ()
idle = void $ move zero

askenv :: (Environment -> a) -> Behavior a
askenv f = fmap f getenv
