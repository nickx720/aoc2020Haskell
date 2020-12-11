module Days.Day07 (runDay) where

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
import Data.Functor
import Control.Applicative
import Util.Parsers (around)
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = Map.fromList <$> rule `sepBy` endOfLine
    where
        color = do
            (adj,col) <- many1 letter `around` space
            return (adj ++ " " ++ col)
        rule = do
            container <- color
            string " bags contain "
            rules <-
                choice
                  [ string "no other bags" $> [],
                  bag `sepBy1` (string ", ")
                  ]
            char '.'
            return (container,rules)
        bag = do
            quant <- decimal
            space
            col <- color
            space
            string "bags" <|> string "bag"
            return (quant,col)

------------ TYPES ------------
type Color = String

type BagRules = Map Color ([(Int,Color)])

type Input = BagRules

type OutputA = Int

type OutputB = Int

------------ PART A ------------
validContainers :: BagRules -> Color -> [Color]
validContainers rs col = case validImmediateContainers col of
    [] -> [col]
    ls -> nub . (col :) . concat . fmap (validContainers rs) $ ls
    where
        validImmediateContainers col = 
            Map.keys . Map.filter ((col `elem`) . (fmap snd)) $ rs
partA :: Input -> OutputA
partA input = 
    length
        . (delete "shiny gold")
        $ validContainers input "shiny gold"

------------ PART B ------------
totalBags :: BagRules -> Color -> Int
totalBags rs col =
    sum
    . fmap (\(q,c)-> q+(q * totalBags rs c))
    $ rs Map.! col

partB :: Input -> OutputB
partB input = totalBags input "shiny gold"
