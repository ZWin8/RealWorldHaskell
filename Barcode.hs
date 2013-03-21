{-import Data.Array (Array(..), (!), bounds, 
					elems, indices, ixmap, listArray)-}
import Data.Array 
import Data.Word
import Control.Applicative((<$>))
import Data.Char (digitToInt)
import Nat.Help((*+>),(+*>),sortArray,sortArrayBy)
import Data.List
import Data.Ratio
import qualified Data.Map as M
{-import Control.Monad (forM_)
import Data.Ix (Ix(..))
import Data.List (foldl', group, sort, sortBy, tails)
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ratio (Ratio)
import Data.Word (Word8)
import System.Environment (getArgs)
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M-}

leftOddList, rightList, leftEvenList, parityList :: [String]
leftOddList = ["0001101", "0011001", "0010011","0111101", "0100011",
			   "0110001", "0101111", "0111011","0110111", "0001011"]

rightList = map complement <$> leftOddList
			where
				complement '1' = '0'
				complement '0' = '1'

leftEvenList = map reverse rightList

parityList = ["111111", "110100", "110010", "110001", "101100",
			  "100110", "100011", "101010", "101001", "100101"]

-------------------------------------------------------------------------
listToArray :: [a] -> Array Int a
listToArray xs = listArray (0, l-1) xs
		where l = length xs

leftOddCodes, leftEvenCodes, rightCodes, parityCodes::Array Int String
leftOddCodes = listToArray leftOddList
leftEvenCodes = listToArray leftEvenList
rightCodes = listToArray rightList
parityCodes = listToArray parityList
-------------------------------------------------------------------------

outerGuard = "101"
centerGuard = "01010"

-------------------------------------------------------------------------
encodeEAN13 :: String -> String
encodeEAN13 = concat . encodeDigits . map digitToInt

encodeDigits :: [Int] -> [String]
encodeDigits s@(first:rest) =
		outerGuard : lefties ++ centerGuard : righties ++ [outerGuard]
	where 	
		(left, right) = splitAt 5 rest
		lefties = zipWith leftEncode (parityCodes ! first) left
		righties = map rightEncode (right ++ [checkDigit s])

leftEncode :: Char -> Int -> String
leftEncode '1' = (leftOddCodes !)
leftEncode '0' = (leftEvenCodes !)

rightEncode :: Int -> String
rightEncode = (rightCodes !)

checkDigit :: (Integral a) => [a] -> a
checkDigit ds = 10 - (sum products `mod` 10)
	where
		products = mapEveryOther (*3) (reverse ds)
		mapEveryOther f = zipWith ($) (cycle [f, id]) 

-------------------------------------------------------------------------
data Bit = Zero | One deriving (Eq, Show)

foldA :: Ix k => (s -> b -> s) -> s -> Array k b -> s
foldA f s= foldl' f s . elems

foldA1 :: Ix k => (a -> a -> a) -> Array k a -> a
foldA1 f a = foldA f (a ! fst (bounds a)) a 

threshold :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold n a = binary <$> a
    where binary i | i < pivot = Zero
                   | otherwise = One
          pivot = round $ least + (greatest - least) * n
          least = fromIntegral $ choose (<) a
          greatest = fromIntegral $ choose (>) a
          choose f = foldA1 $ \x y -> if f x y then x else y

threshold' :: (Ix k, Integral a) => Double -> Array k a -> Array k Bit
threshold' n a = binary <$> a
    where binary i | i < pivot = Zero
                   | otherwise = One
          pivot = round $ least + (greatest - least) * n
          least = fromIntegral $ sortedArray  ! (fst (bounds a))
          greatest  = fromIntegral $ sortedArray  ! (snd (bounds a))
          sortedArray = sortArray a



-------------------------------------------------------------------------
type Run = Int
type RunLength a = [(Run,a)]

runLengths :: Eq a => [a] -> [Run]
runLengths = fmap length . group

runLength :: Eq a => [a] -> RunLength a
runLength = (*+>) zip runLengths
------------------------------------------------------------------------
type Score = Ratio Int
type ScoreTable = [[Score]]

scaleToOne :: [Run] -> [Score]
scaleToOne xs = map (% sum xs) xs

asSRL :: [String] -> ScoreTable
asSRL = map (scaleToOne.runLengths)

leftOddSRL = asSRL leftOddList
leftEvenSRL = asSRL leftEvenList
rightSRL= asSRL rightList
paritySRL = asSRL parityList

distance :: [Score] -> [Score] -> Score
distance a b = sum . map abs $ zipWith (-) a b

----------------------------------------------------------------------
type Digit = Word8
bestScore :: ScoreTable -> [Run] -> [(Score, Digit)]
bestScore srl ps = take 3 . sort $ scores
          where scores = [(distance d (scaleToOne ps), n) | d <- srl , n <- [0..9]]

----------------------------------------------------------------------
data Parity a = Odd a | Even a | None a deriving (Show)

fromParity :: Parity a -> a
fromParity (Even a) = a
fromParity (Odd a) = a
fromParity (None a) = a

parityMap :: (a->b) -> Parity a -> Parity b
parityMap f (Even a) = Even (f a)
parityMap f (Odd a) = Odd (f a)
parityMap f (None a) = None (f a)

instance Functor Parity where
         fmap = parityMap

on :: (a->a->b) -> (c->a) -> c -> c -> b
on f g x y = f (g x) (g y)

compareWithoutParity :: (Ord a) => Parity a -> Parity a -> Ordering
compareWithoutParity = compare `on` fromParity

----------------------------------------------------------------------

bestLeft,bestRight :: [Run] -> [Parity (Score, Digit)]
bestLeft ps = sortBy compareWithoutParity
            (map Odd $ bestScore leftOddSRL ps)++
            (map Even $ bestScore leftEvenSRL ps)
bestRight = map None . bestScore rightSRL


chunkWith :: ([a] -> ([a], [a])) -> [a] -> [[a]]
chunkWith _ [] = []
chunkWith f a = let (h, hs) = f a
                in h:chunkWith f hs

chunkOf :: Int -> [a] -> [[a]]
chunkOf n = chunkWith (splitAt n)

candidateDigits :: RunLength Bit -> [[Parity Digit]]
candidateDigits ((_, One):_) = []
candidateDigits rle | length rle < 59 = []
candidateDigits rle 
                    | any null match = []
                    | otherwise = map (map (fmap snd)) match
                  where 
                    match = map bestLeft left ++ map bestRight right
                    left = chunkOf 4 . take 24 . drop 3 $ runLengths
                    right = chunkOf 4. take 24 . drop 32 $ runLengths
                    runLengths = map fst rle

{-
        candidateDigits input
        input = zip (runLengths $ encodeEAN13 "978013211467") (cycle [Zero, One])
-}

----------------------------------------------------------------------
type Map a = M.Map Digit [a]
type DigitMap = Map Digit
type ParityMap = Map (Parity Digit)

updateMap :: Parity Digit     -- ^ new digit
          -> Digit            -- ^ existing key
          -> [Parity Digit]   -- ^ existing digit sequence
          -> ParityMap        -- ^ map to update
          -> ParityMap
updateMap digit key seq = insertMap key (fromParity digit) (digit:seq)

insertMap :: Digit -> Digit -> [a] -> Map a -> Map a
insertMap key digit val m = val `seq` M.insert key' val m
          where key' = (key + digit) `mod` 10

useDigit :: ParityMap -> ParityMap -> Parity Digit -> ParityMap
useDigit old new digit =
         new `M.union` M.foldWithKey (updateMap digit) M.empty old

{-
M.foldWithKey (updateMap (Even 8)) M.empty pm
let pm = M.fromList [(4, [Even 1, Odd 2])]  :: ParityMap
-}
