module Days.Day10 (runDay,combinationsOfDiffList) where

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
import Util.Util
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = decimal `sepBy` endOfLine

------------ TYPES ------------
type Input = [Int]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA input =
    let diffList = 
            (\ls-> zipWith (flip (-)) (0: ls) ls)
            . sort
            $ input 
    in ((+1) . length . filter (==3) $ diffList) * (length . filter (==1) $ diffList)

------------ PART B ------------
combinationsOfDiffList :: [Int] -> Int
combinationsOfDiffList ls = case ls of
    [] -> 1
    _ : [] -> 1
    l : l': ls' ->
        combinationsOfDiffList (l': ls')
            + (if l + l' <= 3
                    then combinationsOfDiffList ((l+l') : ls')
                    else 0
               )
partB :: Input -> OutputB
partB input=
    let diffList = 
            chunksByPredicate (/=3)
                . (\ls -> zipWith (flip (-)) (0:ls) ls)
                . sort
                $ input
    in product $ fmap combinationsOfDiffList diffList 
