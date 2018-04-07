module Data.Strinteger where

import Data.Maybe
-- You might need to use intercalate and splitOn (similar to words/unwords)
import Data.List (intercalate)
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

-- | Vraci nejvyssi rozsah
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
-- TODO: implement String->Integer translation
engNumeral2Integer :: String -> Maybe Integer
engNumeral2Integer enNums  = undefined

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
    compare = undefined

instance Num Strinteger where
    (+) = undefined
    (*) = undefined
    negate = undefined
    abs = undefined
    signum = undefined
    fromInteger = undefined

instance Enum Strinteger where
    toEnum = undefined
    fromEnum = undefined

instance Real Strinteger where
    toRational = undefined

instance Integral Strinteger where
    quotRem = undefined
    toInteger = undefined
