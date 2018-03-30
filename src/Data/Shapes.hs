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
  valid = undefined

-- TODO: complete instances for each type to check validity by `valid` function
instance Validable Circle where
  valid (Circle ciRadius)= ciRadius > 0

instance Validable Triangle where
  valid (EquilateralTriangle a) =  a > 0
  valid (IsoscelesTriangle itBase itLeg) = (itBase > 0) && (itLeg > 0) && (2*itLeg > itBase)
  valid (ScaleneTriangle a b c) = (a > 0) && (b > 0) && (c > 0) && (a + b > c) && (a + c > b) && (c + b > a)

instance Validable Quadrilateral where
  valid (Square a) = a > 0
  valid (Rectangle a b) = (a > 0) && (b > 0)

-- TODO: create appropriate typeclass for 2D shapes (subclass of Validable)
-- TODO: write instances for the types to compute circumference and area

-- Note: this dummy functions should be placed in typeclass
area = undefined
circumference = undefined
