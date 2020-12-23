module Days.Day15 (runDay) where

{- ORMOLU_DISABLE -}


import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import qualified Data.Vector.Unboxed.Mutable as MVec
import Control.Monad.State
import Control.Monad.ST
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` char ','

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
playGame::[Int] -> Int-> Int
playGame startingNumbers target = runST $ do
    prevOccurenceVec <- MVec.replicate target (0::Int)
    sequence_ $
        fmap (uncurry $ MVec.write prevOccurenceVec) (zip startingNumbers [1..])
    let rounds = [length startingNumbers + 1..target]
    let performRound prevVal index = do
            prevOccurence <- MVec.read prevOccurenceVec prevVal
            ( return $ case prevOccurence of
                0 -> 0
                e -> (index - e - 1)
                )
                <* MVec.write prevOccurenceVec prevVal (index-1)
    foldM performRound (last startingNumbers) rounds
partA :: Input -> OutputA
partA input = playGame input 2020

------------ PART B ------------
partB :: Input -> OutputB
partB input = playGame input 30_000_000
