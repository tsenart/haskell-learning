module Main where

import Data.Maybe (catMaybes)
import Data.Foldable (foldl')
import System.Random (RandomGen, randomR, next, mkStdGen)
import System.Random.Shuffle (shuffle')
import System.Environment (getArgs)
import System.Exit (die)
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
type Moves        = Map.Map Alien [Move]
data City         = City CityName Population Neighborhood deriving (Show, Eq, Ord)
type CityMap      = Map.Map CityName City

-- | Simulates an invasion of an alien population with max number of individual alien moves.
simulate :: RandomGen gen => gen -> Int -> CityMap -> (CityMap, gen)
simulate rng limit m = step (moves rng m) limit Map.empty m

-- | Advances the simulation until no more moves are possible or all aliens have
-- moved at least limit times.
step :: RandomGen gen => (Moves, gen) -> Int -> Moves -> CityMap -> (CityMap, gen)
step (smvs, rng) limit mvs cm
  | null smvs = (cm, rng)
  | not (null mvs) && all ((>= limit) . length) mvs = (cm, rng)
  | otherwise = step (moves rng cm') limit mvs' cm'
  where cm'  = fight $ apply cm smvs
        mvs' = Map.unionWith (++) mvs smvs

-- Evaluates and destroys the cities which have more than one alien in them.
fight :: CityMap -> CityMap
fight = Map.filter crowded where
  crowded (City _ p _) = length p > 1

-- Returns the given city map with the given alien moves applied.
apply :: CityMap -> Moves -> CityMap
apply = foldl' $ foldl' mv where
  add alien (City n p ns) = City n (p `union` [alien]) ns
  del alien (City n p ns) = City n (p \\      [alien]) ns
  mv m' (from, alien, (_, to)) = Map.adjust (add alien) to $
                                 Map.adjust (del alien) from m'

-- | Generates possible alien moves from a given city map.
moves :: RandomGen gen => gen -> CityMap -> (Moves, gen)
moves rng m = (Map.fromList $ zip aliens ((:[]) <$> moves'), rng') where
  alien (_, alien', _) = alien'
  aliens = alien <$> moves'
  moves' = catMaybes $ fst <$> mvs
  rng'   = last $ snd <$> mvs
  mvs    = move rng <$> cities
  cities = Map.elems m

-- | Returns an alien move from a given city.
move :: RandomGen gen => gen -> City -> (Maybe Move, gen)
move rng (City _ [] _) = (Nothing, rng)
move rng (City _ _ []) = (Nothing, rng)
move rng (City n p ns) = (Just (n, alien, neighbour), rng'') where
  (alien,     rng')  = pick rng  p
  (neighbour, rng'') = pick rng' ns

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

encodeCityMap :: CityMap -> T.Text
encodeCityMap m = T.unlines (encodeCity <$> Map.elems m)

encodeCity :: City -> T.Text
encodeCity (City n _ ns) = T.unwords (n : (encodeNeighbour <$> ns))

cityDestroyed :: City -> T.Text
cityDestroyed (City n p _) = T.pack $ show n ++ " has been destroyed by " ++ show p

encodeNeighbour :: Neighbour -> T.Text
encodeNeighbour (dir, name) = T.concat [T.pack $ show dir, "=", name]

run :: Int -> Int -> IO ()
run seed population = do
  input <- TIO.getContents
  let cm = parseCityMap input
  let rng = mkStdGen seed
  let (populated, rng') = populate rng [1..population] cm
  let (cm', _) = simulate rng' 10000 populated
  TIO.putStr $ encodeCityMap cm'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [seed, population] -> run (read seed :: Int) (read population :: Int)
    _                  -> die "Usage: simulien <seed> <population>"
