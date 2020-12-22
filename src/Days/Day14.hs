module Days.Day14 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List as L
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
import Data.Functor (($>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = value Vec.empty
    where
        value mask = 
            choice 
                [ endOfInput $> [],
                newMask,
                instruction mask
                ]
        newMask = do
                string "mask = "
                mask <- 
                    Vec.fromList
                        <$> ( count
                                36
                                $ choice
                                  [
                                      char 'X' $> Nothing,
                                      char '1' $> Just True,
                                      char '0' $> Just False 
                                  ]
                            )
                endOfLine 
                value mask
        instruction mask = do
                string "mem["
                pos <- decimal
                string "] = "
                val <- decimal
                endOfLine 
                (:) <$> (return (mask, (pos, val))) <*> value mask

------------ TYPES ------------
type Mask = Vector (Maybe Bool)

type Input = [(Mask, (Int , Int))]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
numberise:: Vector Int -> Int
numberise vec = 
    sum . Vec.zipWith (*) vec
     . Vec.fromList
     . reverse
     . L.take 36
     $ fmap (2 ^) [0..]

vectorise:: Int -> Vector Int
vectorise = 
    Vec.fromList
        .reverse
        . L.take 36
        . unfoldr (\n -> if n `mod` 2 ==1 then Just (1,n `div` 2) else Just (0, n `div` 2))

maskValue:: Mask -> Int -> Int
maskValue mask = 
    numberise
        . Vec.zipWith (\m v -> maybe v fromEnum m) mask
        . vectorise 

partA :: Input -> OutputA
partA = 
    sum . Map.elems
        .foldl'
        (\mem (mask, (pos,val)) -> 
            Map.insert
                pos
                (maskValue mask val)
                mem
        )
        Map.empty

------------ PART B ------------
maskPosition::Mask -> Int -> [Int]
maskPosition mask = fmap numberise . sequence .applyMask mask . vectorise
    where
        applyMask = 
            Vec.zipWith 
                (\m v -> case m of
                    Just True -> [1]
                    Just False -> [v]
                    Nothing -> [0,1])

partB :: Input -> OutputB
partB = 
    sum . Map.elems
        .foldl'
            (\mem (mask, (pos, val)) ->
                foldr 
                    (\pos' ->
                        Map.insert pos' val
                    )
                    mem
                    (maskPosition mask pos)
            )
            Map.empty