module Main where

import Control.Exception (IOException)
import Control.Exception.Base (catch)
import Data.Maybe (catMaybes)
import System.Random (RandomGen, randomR, next)
import System.Random.Shuffle (shuffle')
import Data.List (union, (\\))
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

data Direction    = North | East | South | West deriving (Show, Eq, Ord)
type CityName     = T.Text
type Neighbour    = (Direction, CityName)
type Neighborhood = [Neighbour]
type Alien        = (Int, Int)
type Population   = [Alien]
type Move         = (CityName, Population, Neighbour)
data City         = City CityName Population Neighborhood deriving (Show, Eq, Ord)
type CityMap      = Map.Map CityName City

-- TODO Find data structure that sorts cities by number of remaining aliens. Heap?
-- TODO Terminate simulation when max moves are reached per alien

-- | Simulates an invasion of an alien population with max number of individual alien moves.
-- Assumes the given city map is already populated.
simulate :: RandomGen gen => gen -> Int -> CityMap -> (CityMap, gen)
simulate = undefined

-- Computes one iteration of the simulation, returning the resulting city map as well
-- as the applied alien moves.
step :: RandomGen gen => gen -> Int -> CityMap -> (CityMap, [Move], gen)
step rng _ m = (step', moves', rng') where
  (moves', rng') = moves rng m
  step' = foldr adjust m moves'
  add p (City n p' ns) = City n (p' `union` p) ns
  del p (City n p' ns) = City n (p' \\ p) ns
  adjust (from, p, (_, to)) m' = Map.adjust (add p) to $
                                 Map.adjust (del p) from m'

-- | Generates a list of alien moves from a given city map.
moves :: RandomGen gen => gen -> CityMap -> ([Move], gen)
moves rng m = (catMaybes $ fst <$> moves', last $ snd <$> moves') where
  moves' = move <$> Map.elems m
  move (City _ [] _) = (Nothing, rng)
  move (City _ _ []) = (Nothing, rng)
  move (City n p ns) = (Just (n, [alien], neighbour), rng') where
    (alien, _)        = pick rng p
    (neighbour, rng') = pick rng ns

pick :: RandomGen gen => gen -> [a] -> (a, gen)
pick rng xs = (xs !! i, rng') where
  (i, rng') = randomR (0, length xs - 1) rng

-- | Populates a CityMap with the given Population placed at random cities.
populate :: RandomGen gen => gen -> Population -> CityMap -> (CityMap, gen)
populate rng p m = (Map.fromListWith merge $ zip names populated, snd $ next rng) where
  merge (City name lp ns) (City _ rp _) = City name (lp ++ rp) ns
  names = cityName <$> cities
  populated = zipWith join population cities
  population = [[alien] | alien <- p] ++ replicate (length m - length p) []
  join aliens (City n p' ns) = City n (aliens ++ p') ns
  cities = cycle $ shuffle' (Map.elems m) (length m) rng

cityName :: City -> CityName
cityName (City n _ _ ) = n

-- | Reads a city map from a file.
cityMapFromFile :: FilePath -> IO (Either String CityMap)
cityMapFromFile fp = fmap parseCityMap <$> try' (TIO.readFile fp) where
  try' a = (Right <$> a) `catch` err
  err :: IOException -> IO (Either String T.Text)
  err = return . Left . show

-- | Parses a city map from a new line separated list of cities.
parseCityMap :: T.Text -> CityMap
parseCityMap t = Map.fromList $ zip names cities where
  cities = catMaybes $ parseCity <$> T.lines t
  names = cityName <$> cities

-- | Parses a city assuming a space separated list of tokens where the
-- first token is the city name and the following tokens are of the form
-- direction=name.
parseCity :: T.Text -> Maybe City
parseCity = city . T.words where
  city [] = Nothing
  city (name : directions) = Just $ City name [] neighborhood where
    neighborhood = catMaybes $ zipWith neighbour dirs names where
      neighbour Nothing    _ = Nothing
      neighbour (Just dir) n = Just (dir, n)
      dirs = parseDirection . head <$> tokens
      names = last <$> tokens
      tokens = filter ((== 2) . length) $ T.splitOn "=" <$> directions

parseDirection :: T.Text -> Maybe Direction
parseDirection t = case T.toLower t of
  "north" -> Just North
  "east"  -> Just East
  "south" -> Just South
  "west"  -> Just West
  _       -> Nothing

main :: IO ()
main = undefined
