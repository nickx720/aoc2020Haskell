module Days.Day13 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Maybe

import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
import Data.Functor
import Data.Function (on)
import Control.Applicative ((<|>))
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = 
 (,) 
    <$> decimal <* endOfLine 
    <*> (char 'x' $> Nothing <|> Just <$> decimal ) `sepBy` char ','

------------ TYPES ------------
type Input = (Int, [Maybe Int])

type OutputA = Int

type OutputB = Int

------------ PART A ------------
partA :: Input -> OutputA
partA (time,ids)=
    (\idPair -> fst idPair * snd idPair)
     . minimumBy  (compare `on` snd)
     . fmap (\id-> (id,id - (time `mod` id)))
     . catMaybes 
     $ ids 

------------ PART B ------------
chineseSieve :: [(Int, Int)] -> Int
chineseSieve congruences = 
    let (firstA, firstN) = head congruences
        in chineseSieve' firstA firstN (tail congruences)
    where
        chineseSieve' a _ [] =a
        chineseSieve' a n ((a', n'): congs) =
            let nextA = 
                    fromJust $
                        find ((== a') . (`mod` n')) [a, a+n..]
            in chineseSieve' nextA (n* n') congs

partB :: Input -> OutputB
partB (_, ids) =
    let congruences =
         reverse
          . sortBy (compare `on` snd) 
          . fmap (\(a,n) ->((n-a) `mod` n,n))
          . catMaybes
          . fmap sequence
          $ zip [0..] ids
    in chineseSieve congruences 
