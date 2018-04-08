module Data.Strinteger where

import Data.Maybe
-- You might need to use intercalate and splitOn (similar to words/unwords)
import Data.List (intercalate, elemIndex, isPrefixOf)
import Data.List.Split (splitOn)

-- Use Data.Strinteger.Helpers submodule
-- In case of need, feel free to change or enhance Helpers or create own
-- submodule
--
-- DO NOT HARDCODE ANY STRINGs/CHARs IN THIS MODULE!
import qualified Data.Strinteger.Helpers as SH

-- | Strinteger type (wrapper) for English numerals
newtype Strinteger = Strinteger String
                   deriving (Show, Read)

instance Bounded Strinteger where
   maxBound = pack SH.highestPossible
   minBound = negate maxBound

   -- | Pack Integer into Strinteger (English numeral string)
pack :: Integer -> Strinteger
pack integer = Strinteger $ fromMaybe err (integer2EngNumeral integer)
               where
                 err = error $ SH.messageBadInteger integer

-- | Unpack Strinteger (English numeral string) to Integer
unpack :: Strinteger -> Integer
unpack (Strinteger numeral) = fromMaybe err (engNumeral2Integer numeral)
                              where
                                err = error $ SH.messageBadNumeral numeral


-- | Helper function
replace :: Eq a => [a] -> [a] -> [a] -> [a]
replace [] _ _ = []
replace s find repl =
  if take (length find) s == find
  then repl ++ (replace (drop (length find) s) find repl)
  else [head s] ++ (replace (tail s) find repl)

-- | Translate Integer to String (if possible)
integer2EngNumeral :: Integer -> Maybe String
integer2EngNumeral number
  | abs(number) > SH.highestPossible = error ("Uncovertable Integer: " ++ show (number))
  | number < 0 = Just ("minus " ++ (fromJust (integer2EngNumeral (abs number))))
  | number < 20 = (SH.num2word 1 number)
  | (number < 100) && (number `mod` 10 == 0) = (SH.num2word 10 (number `div` 10))
  | (number < 100) = Just ((fromJust (SH.num2word 10 (number `div` 10))) ++ "-" ++ (fromJust (integer2EngNumeral (number `mod` 10))))
  | otherwise = Just ((fromJust (integer2EngNumeral leftNumber) ++ " " ++ (fromJust (SH.num2word maxScale 0))) ++ lastNumberString restNumber)
    where
      maxScale = heighestScaleWord number (heighestScale number (10^63))
      leftNumber = number `div` maxScale
      restNumber = number - (leftNumber * maxScale)
      lastNumberString :: Integer -> String
      lastNumberString restNumber
        | restNumber == 0 = ""
        | otherwise = " " ++ (fromJust (integer2EngNumeral restNumber))
      heighestScaleWord :: Integer ->Integer -> Integer
      heighestScaleWord number scale = case word of
        Nothing -> heighestScaleWord number (scale `div` 10)
        _ -> scale
        where
          word = SH.num2word scale 0
      heighestScale :: Integer -> Integer -> Integer
      heighestScale n s
          | n `div` s == 0 = heighestScale n (s `div` 10){- sniz rozsah-}
          | otherwise = s


-- | Translate String to Integer (if possible)
engNumeral2Integer :: String -> Maybe Integer
engNumeral2Integer enNums
  | isPrefixOf "minus " enNums = Just ((-1) * fromJust ((engNumeral2Integer (replace enNums "minus " ""))))
  | hasError enNums = error (SH.messageBadNumeral enNums)
  | enNums == "zero" = Just 0
  | otherwise = Just (numsSum (wordToNums enNums))
  where
    hasError input
      | elem "zero" splitted && (length splitted /= 1) = True
      | otherwise = (length splitted) /= (length (filter (wordExists) splitted))
      where
        splitted = splitOn " " replaced
        replaced = replace input "-" " "
        wordExists ::  String -> Bool
        wordExists w = case SH.word2num w of
          Nothing -> False
          _ -> True
    numsSum :: [Integer] -> Integer
    numsSum [] = 0
    numsSum arr@(x:xs) = if(xs /= []) then ((numsSum leftArr) * maxNum) + (numsSum rightArr) else x
      where
        maxNum = maximum arr
        maxIndex = fromJust (elemIndex maxNum arr)
        leftArr = take maxIndex arr
        rightArr = drop (maxIndex + 1) arr
    wordToNums :: String -> [Integer]
    wordToNums enNums = map (wordToNum) (splitOn " " enNums)
    wordToNum :: String -> Integer
    wordToNum word = case wordMaybe of
      Nothing -> sum (map (wordToNum) (splitOn "-" word))
      _ -> ((if number == 0 then 1 else number) * scale)
      where
        wordMaybe = SH.word2num word
        scale = fst (fromJust wordMaybe)
        number = snd (fromJust wordMaybe)

instance Eq Strinteger where
    (Strinteger s1) == (Strinteger s2) = s1 == s2

instance Ord Strinteger where
    compare s1 s2 = compare (unpack s1) (unpack s2)

instance Num Strinteger where
    (+) s1 s2 = pack ((unpack s1) + (unpack s2))
    (*) s1 s2 = pack ((unpack s1) * (unpack s2))
    (-) s1 s2 = pack ((unpack s1) - (unpack s2))
    negate s = pack ((unpack s) * (-1))
    abs s = pack (abs (unpack s))
    signum s = pack (signum (unpack s))
    fromInteger s = pack (fromInteger s)

instance Enum Strinteger where
    toEnum s = pack (toInteger s)
    fromEnum s = fromInteger (unpack s)

instance Real Strinteger where
    toRational s = toRational (unpack s)

instance Integral Strinteger where
    quotRem s1 s2 = (pack (n1 `quot` n2), pack (n1 `rem` n2))
      where
        n1 = unpack s1
        n2 = unpack s2
    toInteger s = unpack s
