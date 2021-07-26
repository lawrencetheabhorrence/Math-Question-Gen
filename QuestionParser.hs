module QuestionParser where
import Data.List
import Data.String.Utils
import qualified Data.Set as Set
import qualified System.Random as Rand

-- a faster version of nub
-- https://github.com/nh2/haskell-ordnub#dont-use-nub
ordNub :: (Ord a) => [a] -> [a]
ordNub l = go Set.empty 1
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

data Question = Question String
data ChoiceSet = ChoiceSet [Choice]

getLabels :: String -> [String] -- returns the formatters of a question template
getLabels = filter (startswith '%') . ordNub . words

intRange :: (Integer, Integer)
intRange = (30, 300)

-- generate the values for the formatters
generateValues :: [String] -> [(String, String)]
generateValues [] = []
generateValues (x:xs) = (x, genVal x):(generateValues xs)
  where 
    genVal ('%':c:cs) =
      case c of
        'n' -> generateName
        't' -> generateTime
        'i' -> generateInt
        'd' -> generateDecimal
        'p' -> generatePercentage

generateTime :: (RandomGen g) => g -> String
generateTime seed = show . fst (Rand.uniformR (1, 12) seed) ++ ":" ++ show . fst (Rand.uniformR (0 60) seed)

generateInt :: (RandomGen g) => g -> String
generateInt seed = show . fst Rand.uniformR intRange 

generateDecimal :: (RandomGen g) => g -> String
generateDecimal seed = show . fst Rand.uniformR (1 :: Double, 30 :: Double) seed

generatePercentage :: (RandomGen g) => g -> String
generatePercentage seed = show . fst Rand.uniformR (1 :: Integer, 99 :: Integer) seed ++ "%"

-- now that we have the generated values in a list of tuples
-- (which is analogous to a hashmap in Java, or a dictionary in Python)
-- we can now substitute it into the template
subValues :: [(String, String)] -> String -> String
subValues dict template = buildResult (words template) ""
  where
    buildResult [] result = result
    buildResult (('%':cs):ws) result = buildResult ws (result ++ maybe "" id $ lookup cs dict)
    buildResult (w:ws) result = buildResult ws (result ++ w)

-- now this is where we handle I/O and random generation
-- both of which we cannot do with pure functions
