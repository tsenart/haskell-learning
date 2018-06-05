module Main where

import qualified Data.Map.Strict as Map
import qualified Data.ByteString as BS
import qualified Data.Text as T
import Data.Maybe (catMaybes)

data Direction    = North | East | South | West deriving (Show)
type CityName     = T.Text
type Neighbour    = (Direction, CityName)
type Neighborhood = [Neighbour]
type Alien        = Int
type Population   = [Alien]
data City         = City CityName Population Neighborhood deriving (Show)
type CityMap      = Map.Map CityName City

-- | Simulates an invasion of an alien population with max number of individual alien moves.
-- Assumes the given city map is already populated.
simulate :: Int -> CityMap -> CityMap
simulate = undefined

populate :: Population -> CityMap -> CityMap
populate p m = snd $ foldr pop (p, Map.empty) m where
  pop (City name p' n) (ps, m')
    | not $ null ps = (tail ps, Map.insert name (City name (head ps : p') n) m')
    | otherwise     = (ps,      Map.insert name (City name p' n) m')

populate' :: Population -> CityMap -> CityMap
populate' p m = Map.fromList $ zip names cities where
  cities = zipWith pop p $ Map.elems m
  pop alien (City n p' ns) = City n (alien:p') ns
  names  = cityName <$> cities

cityName :: City -> CityName
cityName (City n _ _ ) = n

-- | Parses a city map from a line separated list of cities.
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
main = do
  contents <- BS.getContents
  print contents
