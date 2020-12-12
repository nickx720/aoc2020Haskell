module Days.Day08 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Control.Monad.State
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Vec.fromList <$> instruction `sepBy` endOfLine
    where
        instruction = 
            choice
                [
                    string "acc " >> Acc <$> (signed decimal),
                    string "jmp " >> Jmp <$> (signed decimal),
                    string "nop " >> Noop <$> (signed decimal)
                ]

------------ TYPES ------------
data Instruction = Acc Int | Jmp Int | Noop Int deriving (Show, Eq)

data EndCondition = Loop | Terminate deriving (Show, Eq)

data GamesConsole = GamesConsole
    {
        accumulator:: Int,
        program:: Vector Instruction,
        currentPos:: Int,
        prevPos:: Set Int
    }

type ConsoleM = State GamesConsole

type Input = Vector Instruction

type OutputA = Int

type OutputB = Int

------------ PART A ------------
runProgram :: ConsoleM EndCondition
runProgram = do
    pos <- gets currentPos 
    modify (\c -> c { prevPos = Set.insert pos $ prevPos c})
    program <- gets (program)
    let instruction = program Vec.! pos
    case instruction of
        Noop _ -> return ()
        Jmp x -> modify (\c -> c {currentPos = currentPos c + x - 1})
        Acc x -> modify (\c -> c {accumulator = accumulator c + x})
    modify (\c -> c {currentPos = currentPos c + 1})
    newPos <- gets currentPos
    prevPos <- gets prevPos

    if
        | Set.member newPos prevPos -> return Loop
        | newPos >= (Vec.length program) -> return Terminate
        | otherwise -> runProgram

partA :: Input -> OutputA
partA input = 
    accumulator $
        execState
            runProgram
            GamesConsole
                {
                    accumulator = 0,
                    program = input,
                    prevPos = Set.empty,
                    currentPos = 0
                } 

------------ PART B ------------
flipInstruction:: Input -> Int -> Maybe Input
flipInstruction prog index = case prog Vec.! index of
    Acc _ -> Nothing
    Jmp x -> Just $ prog Vec.// [(index, Noop x)]
    Noop x -> Just $ prog Vec.// [(index,Jmp x)]

partB :: Input -> OutputB
partB input = 
    accumulator
        . snd
        . fromJust
        . find ((== Terminate) . fst)
        . fmap (runState runProgram)
        . fmap (\prog -> GamesConsole {accumulator = 0, prevPos = Set.empty, currentPos=0, program = prog})
        . catMaybes
        . fmap (flipInstruction input)
        $ [0..(Vec.length input - 1)]
