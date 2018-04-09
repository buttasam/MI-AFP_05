module StrintegerSpec (spec) where

import Control.Exception
import Test.Hspec

import Data.Strinteger


spec :: Spec
spec = do
    describe "pack" $ do
      it "translates basic numbers" $ do
        pack 0 `shouldBe` Strinteger "zero"
        pack 1 `shouldBe` Strinteger "one"
        pack 7 `shouldBe` Strinteger "seven"
        pack 11 `shouldBe` Strinteger "eleven"
        pack 15 `shouldBe` Strinteger "fifteen"
        pack 18 `shouldBe` Strinteger "eighteen"
      it "translates whole tens" $ do
        pack 10 `shouldBe` Strinteger "ten"
        pack 40 `shouldBe` Strinteger "forty"
        pack 60 `shouldBe` Strinteger "sixty"
        pack 90 `shouldBe` Strinteger "ninety"
      it "translates whole higher whole units" $ do
        pack 100 `shouldBe` Strinteger "one hundred"
        pack 1000 `shouldBe` Strinteger "one thousand"
        pack (10^9) `shouldBe` Strinteger "one billion"
        pack (10^51) `shouldBe` Strinteger "one sexdecillion"
        pack (10^63) `shouldBe` Strinteger "one vigintillion"
      it "translates up to hundred" $ do
        pack 25 `shouldBe` Strinteger "twenty-five"
        pack 31 `shouldBe` Strinteger "thirty-one"
        pack 46 `shouldBe` Strinteger "forty-six"
        pack 59 `shouldBe` Strinteger "fifty-nine"
        pack 66 `shouldBe` Strinteger "sixty-six"
        pack 92 `shouldBe` Strinteger "ninety-two"
      it "translates hundreds" $ do
        pack 125 `shouldBe` Strinteger "one hundred twenty-five"
        pack 301 `shouldBe` Strinteger "three hundred one"
        pack 460 `shouldBe` Strinteger "four hundred sixty"
        pack 519 `shouldBe` Strinteger "five hundred nineteen"
        pack 666 `shouldBe` Strinteger "six hundred sixty-six"
        pack 800 `shouldBe` Strinteger "eight hundred"
      it "translates thousands" $ do
        pack 1025 `shouldBe` Strinteger "one thousand twenty-five"
        pack 2301 `shouldBe` Strinteger "two thousand three hundred one"
        pack 4060 `shouldBe` Strinteger "four thousand sixty"
        pack 5172 `shouldBe` Strinteger "five thousand one hundred seventy-two"
        pack 6600 `shouldBe` Strinteger "six thousand six hundred"
        pack 8000 `shouldBe` Strinteger "eight thousand"
        pack 21011 `shouldBe` Strinteger "twenty-one thousand eleven"
        pack 60000 `shouldBe` Strinteger "sixty thousand"
        pack 75412 `shouldBe` Strinteger "seventy-five thousand four hundred twelve"
        pack 124500 `shouldBe` Strinteger "one hundred twenty-four thousand five hundred"
        pack 721011 `shouldBe` Strinteger "seven hundred twenty-one thousand eleven"
      it "translates milions and billions" $ do
        pack 1256721 `shouldBe` Strinteger "one million two hundred fifty-six thousand seven hundred twenty-one"
        pack 31286721 `shouldBe` Strinteger "thirty-one million two hundred eighty-six thousand seven hundred twenty-one"
        pack 631256761 `shouldBe` Strinteger "six hundred thirty-one million two hundred fifty-six thousand seven hundred sixty-one"
        pack 1492638526 `shouldBe` Strinteger "one billion four hundred ninety-two million six hundred thirty-eight thousand five hundred twenty-six"
        pack 41402638720 `shouldBe` Strinteger "forty-one billion four hundred two million six hundred thirty-eight thousand seven hundred twenty"
      it "translates huge numbers" $ do
        pack 5000000045000000111000000000000000002 `shouldBe` Strinteger "five undecillion forty-five octillion one hundred eleven quintillion two"
        pack (2*10^63 + 256*10^27) `shouldBe` Strinteger "two vigintillion two hundred fifty-six octillion"
      it "translates negative numbers" $ do
        pack (-7) `shouldBe` Strinteger "minus seven"
        pack (-163) `shouldBe` Strinteger "minus one hundred sixty-three"
        pack (-631256717) `shouldBe` Strinteger "minus six hundred thirty-one million two hundred fifty-six thousand seven hundred seventeen"
      it "returns error for illegal numbers" $ do
        evaluate (pack (10^64)) `shouldThrow` errorCall ("Uncovertable Integer: " ++ show (10^64))
        evaluate (pack (-10^64)) `shouldThrow` errorCall ("Uncovertable Integer: " ++ show (-10^64))
        evaluate (pack (-214*10^64 + 482*10^6)) `shouldThrow` errorCall ("Uncovertable Integer: " ++ show (-214*10^64 + 482*10^6))
    describe "unpack (Strinteger " $ do
      it "translates basic numbers" $ do
        unpack (Strinteger "zero") `shouldBe` 0
        unpack (Strinteger "one") `shouldBe` 1
        unpack (Strinteger "seven") `shouldBe` 7
        unpack (Strinteger "eleven") `shouldBe` 11
        unpack (Strinteger "fifteen") `shouldBe` 15
        unpack (Strinteger "eighteen") `shouldBe` 18
      it "translates whole tens" $ do
        unpack (Strinteger "ten") `shouldBe` 10
        unpack (Strinteger "forty") `shouldBe` 40
        unpack (Strinteger "sixty") `shouldBe` 60
        unpack (Strinteger "ninety") `shouldBe` 90
      it "translates whole higher whole units" $ do
        unpack (Strinteger "one hundred") `shouldBe` 100
        unpack (Strinteger "one thousand") `shouldBe` 1000
        unpack (Strinteger "one billion") `shouldBe` 10^9
        unpack (Strinteger "one sexdecillion") `shouldBe` 10^51
        unpack (Strinteger "one vigintillion") `shouldBe` 10^63
      it "translates up to hundred" $ do
        unpack (Strinteger "twenty-five") `shouldBe` 25
        unpack (Strinteger "thirty-one") `shouldBe` 31
        unpack (Strinteger "forty-six") `shouldBe` 46
        unpack (Strinteger "fifty-nine") `shouldBe` 59
        unpack (Strinteger "sixty-six") `shouldBe` 66
        unpack (Strinteger "ninety-two") `shouldBe` 92
      it "translates hundreds" $ do
        unpack (Strinteger "one hundred twenty-five") `shouldBe` 125
        unpack (Strinteger "three hundred one") `shouldBe` 301
        unpack (Strinteger "four hundred sixty") `shouldBe` 460
        unpack (Strinteger "five hundred nineteen") `shouldBe` 519
        unpack (Strinteger "six hundred sixty-six") `shouldBe` 666
        unpack (Strinteger "eight hundred") `shouldBe` 800
      it "translates thousands" $ do
        unpack (Strinteger "one thousand twenty-five") `shouldBe` 1025
        unpack (Strinteger "two thousand three hundred one") `shouldBe` 2301
        unpack (Strinteger "four thousand sixty") `shouldBe` 4060
        unpack (Strinteger "five thousand one hundred seventy-two") `shouldBe` 5172
        unpack (Strinteger "six thousand six hundred") `shouldBe` 6600
        unpack (Strinteger "eight thousand") `shouldBe` 8000
        unpack (Strinteger "twenty-one thousand eleven") `shouldBe` 21011
        unpack (Strinteger "sixty thousand") `shouldBe` 60000
        unpack (Strinteger "seventy-five thousand four hundred twelve") `shouldBe` 75412
        unpack (Strinteger "one hundred twenty-four thousand five hundred") `shouldBe` 124500
        unpack (Strinteger "seven hundred twenty-one thousand eleven") `shouldBe` 721011
      it "translates milions and billions" $ do
        unpack (Strinteger "one million two hundred fifty-six thousand seven hundred twenty-one") `shouldBe` 1256721
        unpack (Strinteger "thirty-one million two hundred eighty-six thousand seven hundred twenty-one") `shouldBe` 31286721
        unpack (Strinteger "six hundred thirty-one million two hundred fifty-six thousand seven hundred sixty-one") `shouldBe` 631256761
        unpack (Strinteger "one billion four hundred ninety-two million six hundred thirty-eight thousand five hundred twenty-six") `shouldBe` 1492638526
        unpack (Strinteger "forty-one billion four hundred two million six hundred thirty-eight thousand seven hundred twenty") `shouldBe` 41402638720
      it "translates huge numbers" $ do
        unpack (Strinteger "five undecillion forty-five octillion one hundred eleven quintillion two") `shouldBe` 5000000045000000111000000000000000002
        unpack (Strinteger "two vigintillion two hundred fifty-six octillion") `shouldBe` (2*10^63 + 256*10^27)
      it "translates negative numbers" $ do
        unpack (Strinteger  "minus seven") `shouldBe` (-7)
        unpack (Strinteger  "minus one hundred sixty-three") `shouldBe` (-163)
        unpack (Strinteger  "minus six hundred thirty-one million two hundred fifty-six thousand seven hundred seventeen") `shouldBe` (-631256717)
      it "returns error for illegal English numerals" $ do
        evaluate (unpack (Strinteger "")) `shouldThrow` errorCall "Illegal English numeral: ''"
        evaluate (unpack (Strinteger "bleh")) `shouldThrow` errorCall "Illegal English numeral: 'bleh'"
        evaluate (unpack (Strinteger "eléven")) `shouldThrow` errorCall "Illegal English numeral: 'eléven'"
        {- evaluate (unpack (Strinteger "thousand")) `shouldThrow` errorCall "Illegal English numeral: 'thousand'" | correct input  -}
        evaluate (unpack (Strinteger "bloody thousand")) `shouldThrow` errorCall "Illegal English numeral: 'bloody thousand'"
        evaluate (unpack (Strinteger "five hundert")) `shouldThrow` errorCall "Illegal English numeral: 'five hundert'"
        evaluate (unpack (Strinteger "fourty-four")) `shouldThrow` errorCall "Illegal English numeral: 'fourty-four'"
        evaluate (unpack (Strinteger "fifty-zero")) `shouldThrow` errorCall "Illegal English numeral: 'fifty-zero'"
        evaluate (unpack (Strinteger "one hundred zero")) `shouldThrow` errorCall "Illegal English numeral: 'one hundred zero'"
        evaluate (unpack (Strinteger "zero million two")) `shouldThrow` errorCall "Illegal English numeral: 'zero million two'"
        evaluate (unpack (Strinteger "minux twenty")) `shouldThrow` errorCall "Illegal English numeral: 'minux twenty'"

    describe "Strinteger instances" $ do
      it "is Ord: compares numbers in strings" $ do
        (Strinteger "sixty-six" > Strinteger "eleven") `shouldBe` True
        (Strinteger "minus sixty-six" > Strinteger "eleven") `shouldBe` False
        (Strinteger "one undecillion" <= Strinteger "three hundred forty-seven") `shouldBe` False
      it "is Num: adds numbers in strings" $ do
        (Strinteger "sixty-six" + Strinteger "eleven") `shouldBe` Strinteger "seventy-seven"
        (Strinteger "minus sixty-six" + Strinteger "five") `shouldBe` Strinteger "minus sixty-one"
        (Strinteger "twenty-five million" + Strinteger "one decillion") `shouldBe` Strinteger "one decillion twenty-five million"
      it "is Num: subtract numbers in strings" $ do
        (Strinteger "sixty-six" - Strinteger "eleven") `shouldBe` Strinteger "fifty-five"
        (Strinteger "minus sixty-six" - Strinteger "five") `shouldBe` Strinteger "minus seventy-one"
        (Strinteger "twenty-five million" - Strinteger "three million seven hundred thousand") `shouldBe` Strinteger "twenty-one million three hundred thousand"
      it "is Num: multiplies numbers in strings" $ do
        (Strinteger "five" * Strinteger "eleven") `shouldBe` Strinteger "fifty-five"
        (Strinteger "minus six" * Strinteger "five") `shouldBe` Strinteger "minus thirty"
        (Strinteger "two hundred seven" * Strinteger "ten") `shouldBe` Strinteger "two thousand seventy"
      it "is Num: has defined absolute value" $ do
        abs (Strinteger "two hundred forty-one") `shouldBe` Strinteger "two hundred forty-one"
        abs (Strinteger "minus ten") `shouldBe` Strinteger "ten"
        abs (Strinteger "zero") `shouldBe` Strinteger "zero"
        abs (Strinteger "minus twenty-nine") `shouldBe` Strinteger "twenty-nine"
      it "is Num: has defined signum" $ do
        signum (Strinteger "five thousand four hundred twelve") `shouldBe` Strinteger "one"
        signum (Strinteger "minus seventeen") `shouldBe` Strinteger "minus one"
        signum (Strinteger "zero") `shouldBe` Strinteger "zero"
      it "is Num: has defined negate" $ do
        negate (Strinteger "five thousand four hundred twelve") `shouldBe` Strinteger "minus five thousand four hundred twelve"
        negate (Strinteger "minus seventeen") `shouldBe` Strinteger "seventeen"
        negate (Strinteger "zero") `shouldBe` Strinteger "zero"
      it "is Num: has defined fromInteger (from numeric literal)" $ do
        (75412 :: Strinteger) `shouldBe` Strinteger "seventy-five thousand four hundred twelve"
        ((-124500) :: Strinteger) `shouldBe` Strinteger "minus one hundred twenty-four thousand five hundred"
        (721011 :: Strinteger) `shouldBe` Strinteger "seven hundred twenty-one thousand eleven"
      it "is Integral: can compute quotient and remainder" $ do
        (Strinteger "eleven" `quotRem` Strinteger "five") `shouldBe` (Strinteger "two", Strinteger "one")
        (Strinteger "seven" `quotRem` Strinteger "minus two") `shouldBe` (Strinteger "minus three", Strinteger "one")
        (Strinteger "minus fifteen" `quotRem` Strinteger "six") `shouldBe` (Strinteger "minus two", Strinteger "minus three")
        (Strinteger "minus ten" `quotRem` Strinteger "minus three") `shouldBe` (Strinteger "three", Strinteger "minus one")
      it "is Integral: has defined toInteger" $ do
        toInteger (Strinteger "seventy-five thousand four hundred twelve") `shouldBe` 75412
        toInteger (Strinteger "minus one hundred twenty-four thousand five hundred") `shouldBe` (-124500)
        toInteger (Strinteger "seven hundred twenty-one thousand eleven") `shouldBe` 721011
      it "is Integral: automatically gets `div` and `mod`" $ do
        (Strinteger "eleven" `div` Strinteger "five") `shouldBe` Strinteger "two"
        (Strinteger "seven" `div` Strinteger "minus two") `shouldBe` Strinteger "minus four"
        (Strinteger "seven" `mod` Strinteger "two") `shouldBe` Strinteger "one"
        (Strinteger "minus ten" `mod` Strinteger "minus three") `shouldBe` Strinteger "minus one"
      it "is Enum: can use succ, pred, dot-dot syntactic sugar" $ do
        succ (Strinteger "zero") `shouldBe` Strinteger "one"
        pred (Strinteger "one thousand") `shouldBe` Strinteger "nine hundred ninety-nine"
        [(Strinteger "three")..(Strinteger "ten")] `shouldBe` [Strinteger "three",Strinteger "four",Strinteger "five",Strinteger "six",Strinteger "seven",Strinteger "eight",Strinteger "nine",Strinteger "ten"]
        [(Strinteger "three"),(Strinteger "seven")..(Strinteger "twenty")] `shouldBe` [Strinteger "three",Strinteger "seven",Strinteger "eleven",Strinteger "fifteen",Strinteger "nineteen"]
        take 10 [(Strinteger "one thousand")..] `shouldBe` [Strinteger "one thousand",Strinteger "one thousand one",Strinteger "one thousand two",Strinteger "one thousand three",Strinteger "one thousand four",Strinteger "one thousand five",Strinteger "one thousand six",Strinteger "one thousand seven",Strinteger "one thousand eight",Strinteger "one thousand nine"]
      it "is Bounded: has maxBound and minBound" $ do
        unpack (maxBound :: Strinteger) `shouldBe` 9999999999999999999999999999999999999999999999999999999999999999
        unpack (minBound :: Strinteger) `shouldBe` (-9999999999999999999999999999999999999999999999999999999999999999)
