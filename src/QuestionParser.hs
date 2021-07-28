module QuestionParser where
import Data.List
import Data.String
import Control.Monad
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

genValue :: (Rand.RandomGen g) => g -> String -> (String, String)
genValue g ('%':c:cs) = ((c:cs), genVal c)
  where 
    genVal c =
      case c of
        'n' -> generateName g
        't' -> generateTime g
        'i' -> generateInt g
        'd' -> generateDecimal g
        'p' -> generatePercentage g
genValue _ _ = ("", "")

generateName :: (Rand.RandomGen g) => g -> String
generateName _ = "bob"

generateTime :: (Rand.RandomGen g) => g -> String
generateTime seed = (show . fst $ Rand.uniformR (1 :: Int, 12 :: Int) seed) ++ ":" ++ (show . fst  $ Rand.uniformR (0 :: Int,  60 :: Int) seed)

generateInt :: (Rand.RandomGen g) => g -> String
generateInt seed = show . fst $ Rand.uniformR intRange seed

generateDecimal :: (Rand.RandomGen g) => g -> String
generateDecimal seed = take 4 $ show . fst $ Rand.uniformR (1 :: Double, 30 :: Double) seed

generatePercentage :: (Rand.RandomGen g) => g -> String
generatePercentage seed = (show . fst $ Rand.uniformR (1 :: Integer, 99 :: Integer) seed) ++ "%"

-- now that we have the generated values in a list of tuples
-- (which is analogous to a hashmap in Java, or a dictionary in Python)
-- we can now substitute it into the template
subValues :: [(String, String)] -> String -> String
subValues dict template = concat . reverse $ buildResult (words template) []
  where
    buildResult [] result = intersperse " " result
    buildResult (('%':cs):ws) result = buildResult ws (maybe "" id (lookup cs dict): result)
    buildResult (w:ws) result = buildResult ws (w : result)

-- handling choice generation
generateChoices :: [String] -> [String]
generateChoices choices = zipWith (++) (map (++ ". ") ["A", "B", "C", "D", "E"]) choices

substituteValues :: String -> IO String
substituteValues q = do
  let formatters = getLabels q
  gens <- replicateM (length formatters) Rand.randomIO
  let seeds = map (Rand.mkStdGen) gens
  let valParams = zip seeds formatters
  let values = [uncurry genValue p | p <- valParams]
  return (subValues values q)

genChoiceRand :: IO [String]
genChoiceRand = do
  gens <- replicateM 4 Rand.randomIO
  let choiceSeeds = map (Rand.mkStdGen) gens
  let choices = (show 1) : [generateInt seed | seed <- choiceSeeds]
  gen <- Rand.randomIO
  let choiceGen = Rand.mkStdGen gen
  let shuffledChoices = randPerm choiceGen choices
  return (generateChoices shuffledChoices)
 
-- shuffles a list
randPerm :: (Rand.RandomGen g) => g -> [a] -> [a]
randPerm _ [] = []
randPerm gen xs = let (n, newGen) = Rand.randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n + 1) xs)

-- now this is where we handle I/O and random generation
-- both of which we cannot do with pure functions
main = do
  result <- substituteValues "%i1 %i2 %d %t3 Camilla's favorite numbers"
  putStrLn result
  choices <- genChoiceRand
  putStrLn $ show choices
