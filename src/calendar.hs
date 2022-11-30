{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

-- import Data.Time.Calendar

import Control.Applicative
import Control.Monad
import Data.Maybe (fromMaybe)

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Doc a
  = Single a
  | HorzComp (Doc a) (Doc a)
  | VertComp (Doc a) (Doc a)
  deriving (Functor, Show)

instance Applicative Doc where
  pure = Single
  Single f <*> a = fmap f a
  HorzComp d1 d2 <*> a = HorzComp (d1 <*> a) (d2 <*> a)
  VertComp d1 d2 <*> a = VertComp (d1 <*> a) (d2 <*> a)

instance Monad Doc where
  (>>=) :: Doc a -> (a -> Doc b) -> Doc b
  Single a >>= f = f a
  HorzComp d1 d2 >>= f = HorzComp (d1 >>= f) (d2 >>= f)
  VertComp d1 d2 >>= f = VertComp (d1 >>= f) (d2 >>= f)

type Year = Integer

data Month
  = Jan
  | Feb
  | Mar
  | Apr
  | May
  | Jun
  | Jul
  | Aug
  | Sep
  | Oct
  | Nov
  | Dec
  deriving (Show, Eq, Enum, Bounded)

data DayOfWeek
  = Sun
  | Mon
  | Tue
  | Wed
  | Thu
  | Fri
  | Sat
  deriving (Show, Eq, Enum, Bounded)

type DayOfMonth = Integer

isLeapYear year
  | year `mod` 4 /= 0 || year `mod` 100 == 0 = False
  | otherwise = True

-- | algorithm to calculate day of the week from: https://cs.uwaterloo.ca/~alopez-o/math-faq/node73.html
dayOfWeek :: Integer -> Month -> DayOfMonth -> DayOfWeek
dayOfWeek yyyy mm dd = [minBound .. maxBound] !! fromInteger x7
 where
  -- take last two digits of the years, devide by 4, discard remainder
  x1 = yyyy `mod` 100 `div` 4
  -- add day of the month
  x2 = x1 + dd
  -- add key value of the month
  x3 = x2 + keyValue
  -- subtract 1 for (Jan or Feb) of a leap year
  x4 = if (mm `elem` [Jan, Feb]) && isLeapYear yyyy then x3 - 1 else x3
  -- centuries adjustment
  x5 = case yyyy `div` 100 of
    20 -> x4 + 6
    17 -> x4 + 4
    18 -> x4 + 2
    19 -> x4
    _ -> x4 + 400
  -- add last two digit of year
  x6 = x5 + yyyy `mod` 100
  -- mod by 7, add +6 since the Sunday begin with 0 instead of 1
  x7 = (x6 + 6) `mod` 7
  -- month's key value table
  keyValue = fromMaybe 0 $ lookup mm $ zip [minBound :: Month .. maxBound] [1, 4, 4, 0, 2, 5, 0, 3, 6, 1, 4, 6]

mkYear :: Monad m => Integer -> m Year
mkYear = pure

monthsInYear :: Monad m => Year -> m [Month]
monthsInYear _ = pure [minBound .. maxBound]

daysInMonth :: (Monad m, Integral Year) => Year -> Month -> m [(DayOfMonth, DayOfWeek)]
daysInMonth year month
  | month `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = pure $ dom 31
  | month `elem` [Apr, Jun, Sep, Nov] = pure $ dom 30
  -- Feb in normal year
  | isLeapYear year = pure $ dom 28
  -- Feb in leap year
  | otherwise = pure $ dom 29
 where
  firstDow = dayOfWeek year month 1
  dow = dropWhile (/= firstDow) $ concat $ repeat [minBound .. maxBound]
  dom n = zip [1 .. n] dow

-- dddd :: Monad m => Month -> m Day

doc :: Doc Integer
doc = HorzComp (Single 5) (Single 100)
docf :: Doc (Integer -> Integer)
docf = VertComp (Single (+ 1)) (Single (+ 2))

main = do
  print $ docf <*> doc
  print $ doc >>= pure (fmap ($ 1) docf)
  print $ dayOfWeek 2022 Dec 12
  print $ dayOfWeek 1889 Jul 7
  print $ do
    year <- mkYear @Doc 2022
    months <- monthsInYear year
    daysInMonth year Dec
  putStrLn "Hello world"
