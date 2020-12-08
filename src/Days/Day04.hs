module Days.Day04 (runDay) where

{- ORMOLU_DISABLE -}
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Set as Set
import qualified Data.Set as Set
import Data.Vector (Vector)
import qualified Data.Vector as Vec
import qualified Util.Util as U
import Data.Text (pack)
import Control.Applicative
import Control.Monad (void, guard)
import Data.Either (isRight)
import qualified Program.RunDay as R (runDay)
import Data.Attoparsec.Text
import Data.Void
{- ORMOLU_ENABLE -}

runDay :: Bool -> String -> IO ()
runDay = R.runDay inputParser partA partB

------------ PARSER ------------
inputParser :: Parser Input
inputParser = passport `sepBy` (string "\n\n")
    where
        passport = 
            Map.fromList <$> do
                entry `sepBy` space
        entry = 
                (,)
                <$> (many1 letter)
                <*> ((char ':') *> many1 (letter <|> digit <|> char '#'))

------------ TYPES ------------
type Input = [Map String String]

type OutputA = Int

type OutputB = Int

------------ PART A ------------
correctFileds:: Map String String -> Bool
correctFileds p = 
    (Set.delete "cid" (Set.fromList . Map.keys $ p))
        == Set.fromList ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

partA :: Input -> OutputA
partA = length . Data.List.filter correctFileds

------------ PART B ------------
inRangeInclusive:: Int -> Int -> Int -> Parser()
inRangeInclusive min max x = guard(x >= min && x <= max)

passportValidators:: Map String (Parser())
passportValidators = 
    Map.fromList
        .zip ["byr","iyr","eyr","hgt","hcl","ecl","pid"]
        $ [
            decimal >>= inRangeInclusive 1920 2002,
            decimal >>= inRangeInclusive 2010 2020,
            decimal >>= inRangeInclusive 2020 2030,
            void
                ( choice
                [
                    decimal >>= inRangeInclusive 150 195 >> string "cm",
                    decimal >>= inRangeInclusive 59 76 >> string "in"
                ]),
            char '#' >> void (count 6 (satisfy $ inClass "0-9a-f")),
            void (choice (fmap string ["amb","blu","brn","gry","grn","hzl","oth"])),
            void (count 9 digit)
        ]
partB :: Input -> OutputB
partB = length . Data.List.filter validPassport . Data.List.filter correctFileds
    where
        validPassport = 
            isRight 
                . sequence
                . Map.intersectionWith
                  (\parser value -> parseOnly (parser <* endOfInput) (pack value))
                  passportValidators
