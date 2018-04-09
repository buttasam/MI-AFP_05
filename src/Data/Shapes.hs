module Data.Shapes where

--------------------------------------------------------------------------------
-- DO NOT CHANGE DATA TYPES DEFINITIONS

newtype Circle = Circle { ciRadius :: Double }
               deriving (Show, Read, Eq)

data Triangle = EquilateralTriangle { etSide :: Double }
              | IsoscelesTriangle { itBase :: Double, itLeg :: Double }
              | ScaleneTriangle { stSideA :: Double, stSideB :: Double, stSideC :: Double }
              deriving (Show, Read, Eq)

data Quadrilateral = Square { sqSide :: Double}
                   | Rectangle { reSideA :: Double, reSideB :: Double }
                   deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

class Validable a where
  valid :: a -> Bool
  area :: a -> Double
  circumference :: a -> Double

instance Validable Circle where
  valid (Circle r)= r > 0
  area (Circle r) = pi * r^2
  circumference (Circle r) = 2 * pi * r

instance Validable Triangle where
  valid (EquilateralTriangle a) =  a > 0
  valid (IsoscelesTriangle itBase itLeg) = (itBase > 0) && (itLeg > 0) && (2*itLeg > itBase)
  valid (ScaleneTriangle a b c) = (a > 0) && (b > 0) && (c > 0) && (a + b > c) && (a + c > b) && (c + b > a)

  area (EquilateralTriangle a)
                  | valid (EquilateralTriangle a) = heronArea a a a
                  | otherwise = 0
  area (IsoscelesTriangle itBase itLeg)
                  | valid (IsoscelesTriangle itBase itLeg) = heronArea itBase itLeg itLeg
                  | otherwise = 0
  area (ScaleneTriangle a b c)
                  | valid (ScaleneTriangle a b c) = heronArea a b c
                  | otherwise = 0

  circumference (EquilateralTriangle a)
                  | valid (EquilateralTriangle a) = 3 * a
                  | otherwise = 0
  circumference (IsoscelesTriangle itBase itLeg)
                  | valid (IsoscelesTriangle itBase itLeg) = itBase + itLeg + itLeg
                  | otherwise = 0
  circumference (ScaleneTriangle a b c)
                  | valid (ScaleneTriangle a b c) = a + b + c
                  | otherwise = 0

heronArea :: Double -> Double -> Double -> Double
heronArea a b c = sqrt (s*(s - a) * (s - b) * (s - c))
                where s = (a + b + c) / 2


instance Validable Quadrilateral where
  valid (Square a) = a > 0
  valid (Rectangle a b) = (a > 0) && (b > 0)
  area (Square a) = a*a
  area (Rectangle a b) = a*b
  circumference (Square a) = 4*a
  circumference (Rectangle a b)
                | valid (Rectangle a b) = 2*a + 2*b
                | otherwise = 0
