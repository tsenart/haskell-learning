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
type Alien        = Int
type Population   = [Alien]
type Move         = (CityName, Alien, Neighbour)
type Moves        = Map.Map Alien Move
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
step :: RandomGen gen => gen -> CityMap -> (CityMap, Moves, gen)
step rng m = (step', mvs, rng') where
  (mvs, rng') = moves rng m
  step' = foldr mv m mvs
  add alien (City n p ns) = City n (p `union` [alien]) ns
  del alien (City n p ns) = City n (p \\      [alien]) ns
  mv (from, alien, (_, to)) m' = Map.adjust (add alien) to $
                                 Map.adjust (del alien) from m'

-- | Generates a list of alien moves from a given city map.
moves :: RandomGen gen => gen -> CityMap -> (Moves, gen)
moves rng m = (Map.fromList $ zip (alien <$> moves') moves', rng') where
  alien (_, alien', _) = alien'
  moves' = catMaybes $ fst <$> mvs
  rng' = last $ snd <$> mvs
  mvs = move rng <$> cities
  cities = Map.elems m


-- | Returns an alien move from a given city.
move :: RandomGen gen => gen -> City -> (Maybe Move, gen)
move rng (City _ [] _) = (Nothing, rng)
move rng (City _ _ []) = (Nothing, rng)
move rng (City n p ns) = (Just (n, alien, neighbour), rng') where
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
