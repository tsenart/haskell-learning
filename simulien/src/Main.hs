module Main where

import Data.Maybe (catMaybes)
import System.Random (StdGen, randomR, mkStdGen)
import System.Random.Shuffle (shuffle)
import System.Environment (getArgs)
import System.IO (stderr)
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
type Counts       = Map.Map Alien Int
data City         = City CityName Population Neighborhood deriving (Show, Eq, Ord)
type CityMap      = Map.Map CityName City

-- | Simulates an invasion of an alien population with max number of individual alien moves.
simulate :: StdGen -> Population -> Int -> CityMap -> (CityMap, [City], StdGen)
simulate rng population limit cm = step (moves rng' populated) limit counts [] populated
  where
    (populated, rng') = populate rng population cm
    counts = Map.fromList $ zip population $ repeat 0

-- | Advances the simulation until no more moves are possible or all aliens have
-- moved at least limit times.
step :: ([Move], StdGen) -> Int -> Counts -> [City] -> CityMap -> (CityMap, [City], StdGen)
step ([], rng) _ _ destroyed cm = (cm, destroyed, rng)
step (mv : _, rng) limit counts destroyed cm
  | not (null counts) && all (>= limit) counts = (cm, destroyed, rng)
  | otherwise = step (moves rng cm') limit (inc mv counts) (destroyed ++ gone) cm'
  where (cm', gone) = fight $ apply cm mv
        inc (_, alien, _) = Map.adjust (+1) alien

-- Evaluates and destroys the cities which have more than one alien in them,
-- returning the resulting CityMap and the list of destroyed cities.
fight :: CityMap -> (CityMap, [City])
fight cm = (survivors, Map.elems destroyed) where
  (destroyed, survivors) = Map.partition crowded cm
  crowded (City _ p _) = length p > 1

-- Returns the given city map with the given alien move applied to the
-- corresponding city. Calling code must ensure the target city exists in
-- the map.
apply :: CityMap -> Move -> CityMap
apply cm (from, alien, (_, to)) = Map.adjust (add alien) to $
                                  Map.adjust (del alien) from cm

-- Add and delete alien population from a given city.
add, del :: Alien -> City -> City
add alien (City n p ns) = City n (p `union` [alien]) ns
del alien (City n p ns) = City n (p \\      [alien]) ns

-- | StdGenerates possible alien moves from a given city map.
moves :: StdGen -> CityMap -> ([Move], StdGen)
moves rng m = shuffle' rng' moves' where
  moves' = catMaybes $ fst <$> mvs
  rng'   = last $ snd <$> mvs
  mvs    = move rng <$> cities
  cities = Map.elems m

-- | Returns an alien move from a given city.
move :: StdGen -> City -> (Maybe Move, StdGen)
move rng (City _ [] _) = (Nothing, rng)
move rng (City _ _ []) = (Nothing, rng)
move rng (City n p ns) = (Just (n, head p', head ns'), rng'') where
  (p', rng')   = shuffle' rng p
  (ns', rng'') = shuffle' rng' ns

-- | Shuffles a list of things, given a RNG.
-- From: https://wiki.haskell.org/Random_shuffle
shuffle' :: StdGen -> [a] -> ([a], StdGen)
shuffle' rng [] = ([], rng)
shuffle' rng xs = (shuffle xs rseq', rng') where
  (rseq', rng') = rseq (length xs) rng

-- The sequence (r1,...r[n-1]) of numbers such that r[i] is an
-- independent sample from a uniform random distribution
-- [0..n-i]
rseq :: Int -> StdGen -> ([Int], StdGen)
rseq n rng
  | null seq' = ([], rng)
  | otherwise = (fst <$> init seq', last $ snd <$> seq')
  where
    seq' = rseq' (n - 1) rng
    rseq' :: Int -> StdGen -> [(Int, StdGen)]
    rseq' 0 g = [(0, g)]
    rseq' i g = (j, g) : rseq' (i - 1) g' where
      (j, g') = randomR (0, i) g

-- | Populates a CityMap with the given Population placed at random cities.
populate :: StdGen -> Population -> CityMap -> (CityMap, StdGen)
populate rng p m = (Map.fromListWith merge $ zip names populated, rng') where
  merge (City name lp ns) (City _ rp _) = City name (lp ++ rp) ns
  names = cityName <$> cities
  populated = zipWith join population cities
  population = [[alien] | alien <- p] ++ replicate (length m - length p) []
  join aliens (City n p' ns) = City n (aliens ++ p') ns
  cities = cycle cities'
  (cities', rng') = shuffle' rng $ Map.elems m

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
encodeCityMap m = T.unlines (encodeCity m <$> Map.elems m)

encodeCity :: CityMap -> City -> T.Text
encodeCity cm (City n _ ns) = T.unwords (n : (encodeNeighbour cm <$> ns))

cityDestroyed :: City -> T.Text
cityDestroyed (City n p _) = T.pack $ show n ++ " has been destroyed by aliens " ++ show p

encodeNeighbour :: CityMap -> Neighbour -> T.Text
encodeNeighbour cm (dir, name)
  | Map.member name cm = T.concat [T.pack $ show dir, "=", name]
  | otherwise = ""

run :: Int -> Int -> IO ()
run seed population = do
  input <- TIO.getContents
  let cm = parseCityMap input
  let rng = mkStdGen seed
  let (cm', destroyed, _) = simulate rng [1..population] 10000 cm
  TIO.hPutStr stderr $ T.unlines $ cityDestroyed <$> destroyed
  TIO.putStr $ encodeCityMap cm'

main :: IO ()
main = do
  args <- getArgs
  case args of
    [seed, population] -> run (read seed :: Int) (read population :: Int)
    _                  -> die "Usage: simulien <seed> <population>"
