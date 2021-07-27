module QuestionParser where
import Data.List
import Data.String
import qualified Data.Set as Set
import qualified System.Random as Rand

-- a faster version of nub
-- https://github.com/nh2/haskell-ordnub#dont-use-nub
-- ordNub :: (Ord a) => [a] -> [a]
-- ordNub l = go Set.empty 1
--  where
--    go _ [] = []
--    go s (x:xs) = if x `Set.member` s then go s xs
--                                      else x : go (Set.insert x s) xs

data Question = Question String ChoiceSet
data ChoiceSet = ChoiceSet [String]

startswith :: (Eq a) => a -> [a] -> Bool
startswith x xs = head xs == x

getLabels :: String -> [String] -- returns the formatters of a question template
getLabels = filter (startswith '%') . nub . words

intRange :: (Integer, Integer)
intRange = (30, 300)

-- generate the values for the formatters
generateValues :: (Rand.RandomGen g) => [String] -> g -> [(String, String)]
generateValues [] _ = []
generateValues (('%':cs):xs) g = (cs, genVal cs):(generateValues xs g)
  where 
    genVal (c:cs) =
      case c of
        'n' -> generateName g
        't' -> generateTime g
        'i' -> generateInt g
        'd' -> generateDecimal g
        'p' -> generatePercentage g
generateValues (_:xs) g = generateValues xs g

generateName :: (Rand.RandomGen g) => g -> String
generateName _ = "bob"

generateTime :: (Rand.RandomGen g) => g -> String
generateTime seed = (show . fst $ Rand.uniformR (1 :: Int, 12 :: Int) seed) ++ ":" ++ (show . fst  $ Rand.uniformR (0 :: Int,  60 :: Int) seed)

generateInt :: (Rand.RandomGen g) => g -> String
generateInt seed = show . fst $ Rand.uniformR intRange seed

generateDecimal :: (Rand.RandomGen g) => g -> String
generateDecimal seed = show . fst $ Rand.uniformR (1 :: Double, 30 :: Double) seed

generatePercentage :: (Rand.RandomGen g) => g -> String
generatePercentage seed = (show . fst $ Rand.uniformR (1 :: Integer, 99 :: Integer) seed) ++ "%"

-- now that we have the generated values in a list of tuples
-- (which is analogous to a hashmap in Java, or a dictionary in Python)
-- we can now substitute it into the template
subValues :: [(String, String)] -> String -> String
subValues dict template = buildResult (words template) ""
  where
    buildResult [] result = result
    buildResult (('%':cs):ws) result = buildResult ws (result ++ " " ++ maybe "" id (lookup cs dict))
    buildResult (w:ws) result = buildResult ws (result ++ " " ++ w)

questionFromTemplate :: (Rand.RandomGen g) => String -> g -> String
questionFromTemplate s g = subValues (generateValues (getLabels s) g) s

-- handling choice generation
generateChoices :: (Show a, Rand.RandomGen g) => a -> g -> (g -> String) -> String
generateChoices a seed generator = unwords $ zipWith (++) (map (++ ". ") ["A", "B", "C", "D", "E"]) ((show a) : (replicate 4 (generator seed)))

-- now this is where we handle I/O and random generation
-- both of which we cannot do with pure functions
main = do
  gen <- Rand.randomIO
  let seed = Rand.mkStdGen gen
  let q = "%i1 %i2 %d %t3 Camilla's favorite numbers"
  putStrLn (questionFromTemplate q seed)
