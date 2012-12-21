module Player (
    PlayerID(..),
    Player(..), Brain(..), BrainCont(..), runBrain,
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
    brain :: Brain
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


data Brain where
    Brain :: {
            memory :: memory,
            defaultBehavior :: Behavior memory (),
            currentBehavior :: Behavior memory ()
        } -> Brain


data BrainCont where
    BrainCont :: {
            contMemory :: memory,
            contDefaultBehavior :: Behavior memory (),
            currentView :: BehaviorView memory ()
        } -> BrainCont


runBrain :: Brain -> BrainCont
runBrain (Brain mem def behav) = BrainCont mem' def cont where
    (cont, mem') = runState (viewT behav) mem


type Behavior memory = ProgramT PlayerAction (State memory)

type BehaviorView memory = ProgramViewT PlayerAction (State memory)


getenv :: Behavior memory Environment
getenv = singleton GetEnv

move :: Vec2 -> Behavior memory Vec2
move = singleton . Move

getRandomR :: Random a => (a, a) -> Behavior memory a
getRandomR = singleton . GetRandomR

idle :: Behavior memory ()
idle = void $ move zero

askenv :: (Environment -> a) -> Behavior memory a
askenv f = fmap f getenv
