module Days.Day09 (runDay) where

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
import Data.List as L
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
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
partA =
    (!! 25)
        . fromJust
        . find
            (\ls->
                ls !! 25
                    `notElem` [ x+y
                                | x <- (L.take 25 ls),
                                  y <- (L.take 25 ls),
                                  x /= y
                              ]
            )
        .tails

------------ PART B ------------
partB :: Input -> OutputB
partB input=
    let target = partA input
        in (\ls -> minimum ls + maximum ls)
            .fromJust
            . find (\ls -> sum ls == target)
            . concatMap tails
            . inits
            $ input
