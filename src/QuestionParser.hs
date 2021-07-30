module QuestionParser where
import Data.List
import Data.String
import Control.Monad
import qualified Data.Set as Set
import qualified System.Random as Rand
import System.IO

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

getLabels :: Char -> String -> [String] -- returns the formatters of a question template
getLabels a s = filter (startswith a) (nub (words s))

intRange :: (Integer, Integer)
intRange = (1, 110)

genValue :: Rand.StdGen -> String -> (String, String)
genValue g ('%':c:cs) = (('%':c:cs), genVal c)
  where 
    genVal c =
      case c of
        't' -> generateTime g
        'i' -> generateInt g
        'd' -> generateDecimal g
        'p' -> generatePercentage g
genValue _ _ = ("", "")

generateTime :: Rand.StdGen -> String
generateTime seed = (show . fst $ Rand.uniformR (1 :: Int, 12 :: Int) seed) ++ ":" ++ (show . fst  $ Rand.uniformR (0 :: Int,  60 :: Int) seed)

generateInt :: Rand.StdGen -> String
generateInt seed = show . fst $ Rand.uniformR intRange seed

generateDecimal :: Rand.StdGen -> String
generateDecimal seed = take 4 $ show . fst $ Rand.uniformR (1 :: Double, 30 :: Double) seed

generatePercentage :: Rand.StdGen -> String
generatePercentage seed = (show . fst $ Rand.uniformR (1 :: Integer, 99 :: Integer) seed) ++ "%"

-- now that we have the generated values in a list of tuples
-- (which is analogous to a hashmap in Java, or a dictionary in Python)
-- we can now substitute it into the template
subValues :: [(String, String)] -> String -> Char -> String
subValues dict template a = concat . reverse $ buildResult (words template) []
  where
    buildResult [] result = intersperse " " result
    buildResult (w:ws) result
      | startswith a w = buildResult ws (maybe "" id (lookup w dict):result)
      | otherwise = buildResult ws (w : result)

-- handling choice generation
generateChoices :: [String] -> [String]
generateChoices choices = zipWith (++) (map (++ ". ") ["A", "B", "C", "D", "E"]) choices

substituteValues :: String -> IO String
substituteValues q = do
  let formatters = getLabels '%' q
  gens <- replicateM (length formatters) Rand.randomIO
  let seeds = map (Rand.mkStdGen) gens
  let valParams = zip seeds formatters
  let values = [uncurry genValue p | p <- valParams]
  return (subValues values q '%')

genChoiceRand :: String -> (Rand.StdGen -> String) -> IO [String]
genChoiceRand correct generator = do
  gens <- replicateM 4 Rand.randomIO
  let choiceSeeds = map (Rand.mkStdGen) gens
  let choices = (correct) : [generator seed | seed <- choiceSeeds]
  gen <- Rand.randomIO
  let choiceGen = Rand.mkStdGen gen
  let shuffledChoices = randPerm choiceGen choices
  return (generateChoices shuffledChoices)
 
-- shuffles a list
randPerm :: Rand.StdGen -> [a] -> [a]
randPerm _ [] = []
randPerm gen xs = let (n, newGen) = Rand.randomR (0, length xs - 1) gen
                      front = xs !! n
                  in front : randPerm newGen (take n xs ++ drop (n + 1) xs)

askInput :: String -> IO (String, String)
askInput s = do
  inp <- getLine
  return (s, inp)

getInputMap :: [String] -> IO [(String, String)]
getInputMap ws = do
  result <- mapM askInput ws
  return result

subInputs :: [(String, String)] -> String -> String
subInputs dict template = subValues dict template '?'

receiveInputs :: String -> IO String
receiveInputs template = do
  inputs <- getInputMap $ getLabels '?' template
  return $ subInputs inputs template

chooseGenerator :: String -> (Rand.StdGen -> String)
chooseGenerator ('t':ws) = generateTime
chooseGenerator ('i':ws) = generateInt
chooseGenerator ('d':ws) = generateDecimal
chooseGenerator ('p':ws) = generatePercentage

getChoices :: IO String
getChoices = do
  putStrLn "Correct Answer: "
  correct <- getLine
  putStrLn "Choose generator (t/i/d/p): "
  generatorCh <- getLine
  let generator = chooseGenerator generatorCh
  choices <- genChoiceRand correct generator
  return $ show $ concat . intersperse " " $ choices

processQuestion :: String -> IO String
processQuestion "\n" = do return "\n"
processQuestion q = do
  result <- substituteValues q
  inp <- receiveInputs result
  choices <- getChoices
  return $ inp ++ "\n" ++ show choices

-- now this is where we handle I/O and random generation
-- both of which we cannot do with pure functions
readNames :: IO [String]
readNames = do
  inh <- openFile "names.txt" ReadMode
  names <- hGetContents inh
  return $ lines names

pickRand :: [a] -> Rand.StdGen -> a
pickRand l gen = l !! rand where
  n = length l
  (rand, _) = Rand.randomR (0, (n-1)) gen

randomName :: IO String
randomName = do
  names <- readNames
  gen <- Rand.randomIO
  let seed = Rand.mkStdGen gen
  return $ pickRand names seed

fillNames :: String -> IO String
fillNames s = do
  let formatters = getLabels '!' s
  names <- replicateM (length formatters) randomName
  let nameMap = zip formatters names
  return $ subValues nameMap s '!'


main = do
  questionPrompt "!1 has a number. When I added this number to %i1 and multiplied the sum by %i2, the final result is ?1. What is my number?"

questionPrompt :: String -> IO String
questionPrompt q = do  
  beforeInputs <- substituteValues q >>= fillNames
  putStrLn beforeInputs
  result <- receiveInputs beforeInputs
  putStrLn result
  choices <- getChoices
  putStrLn $ show choices
  return $ result ++ "\n" ++ choices

getQuestions :: FilePath -> IO [String]
getQuestions path = do
  inh <- openFile path ReadMode
  qs <- hGetContents inh
  return $ lines qs

generateExam :: FilePath -> FilePath -> IO ()
generateExam inpath outpath = do
  qs <- getQuestions inpath
  outh <- openFile outpath WriteMode
  seeds <- replicateM 25 (Rand.getStdGen)
  let qtemps = [pickRand qs g | g <- seeds]
  questions <- mapM questionPrompt qtemps
  mapM (hPutStr outh) questions
  hClose outh
