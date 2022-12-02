{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}

module Main where

import Control.Monad
import Data.List (genericLength, genericReplicate, intersperse)
import Data.Maybe (fromMaybe)
import Text.Printf

class Functor w => Comonad w where
  extract :: w a -> a
  duplicate :: w a -> w (w a)
  duplicate = extend id
  extend :: (w a -> b) -> w a -> w b
  extend f = fmap f . duplicate

data Layout a
  = Simple a
  | Padding String
  | Txt String
  | Horiz [Layout a]
  | Vert [Layout a]
  deriving (Functor, Show)

instance Applicative Layout where
  pure = Simple
  (<*>) :: Layout (a -> b) -> Layout a -> Layout b
  Simple f <*> a = fmap f a
  Padding p <*> _ = Padding p
  Txt s <*> _ = Txt s
  Horiz fs <*> a = Horiz (fmap (<*> a) fs)
  Vert fs <*> a = Vert (fmap (<*> a) fs)

instance Monad Layout where
  (>>=) :: Layout a -> (a -> Layout b) -> Layout b
  Simple a >>= f = f a
  Padding p >>= _ = Padding p
  Txt s >>= _ = Txt s
  Horiz ls >>= f = Horiz (fmap (>>= f) ls)
  Vert ls >>= f = Vert (fmap (>>= f) ls)

simpleL = Simple
horizSimpleL = Horiz . fmap simpleL
gridSimpleL = Vert . fmap horizSimpleL

sepBy :: String -> Layout a -> Layout a
sepBy str (Horiz xs) = Horiz $ intersperse (Padding str) xs
sepBy str (Vert xs) = Vert $ intersperse (Padding str) xs
sepBy _ l = l

padL :: (Integral n) => String -> n -> Layout a -> Layout a
padL str n l@(Horiz xs)
  | n <= len = l
  | otherwise = Horiz $ genericReplicate (n - len) (Padding str) <> xs
 where
  len = genericLength xs
padL _ _ l = l

padR :: (Integral n) => String -> n -> Layout a -> Layout a
padR str n l@(Horiz xs)
  | n <= len = l
  | otherwise = Horiz $ xs <> genericReplicate (n - len) (Padding str)
 where
  len = genericLength xs
padR _ _ l = l

padB :: (Integral n) => String -> n -> Layout a -> Layout a
padB str n l@(Vert xs)
  | n <= len = l
  | otherwise = Vert $ xs <> genericReplicate (n - len) (Padding str)
 where
  len = genericLength xs
padB _ _ l = l

monthInYear :: Monad m => Year -> m [Month]
monthInYear _ = pure [minBound .. maxBound]

-- stdYearLayout :: Year -> Layout Month
stdYearLayout _ =
  --gridSimpleL monthGrid
  Vert . fmap (Horiz . fmap simpleL) $ monthGrid
 where
  monthGrid =
    [ [Jan, Feb, Mar]
    , [Apr, May, Jun]
    , [Jul, Aug, Sep]
    , [Oct, Nov, Dec]
    ]

stdMonthLayout :: Year -> Month -> Layout (DayOfMonth, DayOfWeek)
stdMonthLayout year month =
  padB "   " 7 . Vert . padWeek . fmap Horiz $ grid
 where
  padWeek (x : xs) = (padL "--*-" 7 x : init xs) <> [padR "    " 7 (last xs)]
  padWeek [] = []
  grid = fmap simpleL <$> weekInMonth (dayInMonth year month)

showMonth :: (Show a, PrintfArg a) => Layout a -> [String]
showMonth (Padding p) = [p]
showMonth (Txt str) = [str]
showMonth (Simple a) = [printf "%3d " a]
showMonth (Horiz ls) =
  foldr1 (zipWith (<>)) $ fmap showMonth ls
showMonth (Vert ls) = concatMap showMonth ls

main = do
  putStrLn $ unlines $ showMonth $ fromEnum <$> stdYearLayout 2022
  putStrLn . unlines . showMonth $ do
    -- print $ do
    month <- stdYearLayout 2022
    -- stdMonthLayout 2022 month
    -- month <- pure Jan
    fmap fst $ stdMonthLayout 2022 month
  putStrLn "Hello world"

type Year = Integer

data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
  deriving (Show, Eq, Enum, Bounded)

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
  deriving (Show, Eq, Enum, Bounded)

type DayOfMonth = Integer

isLeapYear :: Integral a => a -> Bool
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

dayInMonth :: Year -> Month -> [(DayOfMonth, DayOfWeek)]
dayInMonth year month
  | month `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = dom 31
  | month `elem` [Apr, Jun, Sep, Nov] = dom 30
  -- Feb in leap year
  | isLeapYear year = dom 28
  -- Feb in normal year
  | otherwise = dom 29
 where
  firstDow = dayOfWeek year month 1
  dow = dropWhile (/= firstDow) $ concat $ repeat [minBound .. maxBound]
  dom n = zip [1 .. n] dow

-- | separate list of day in months into weeks
weekInMonth :: [(DayOfMonth, DayOfWeek)] -> [[(DayOfMonth, DayOfWeek)]]
weekInMonth [] = []
weekInMonth (d : ds) =
  let (w, ws) = break ((== Sun) . snd) ds
   in (d : w) : weekInMonth ws
