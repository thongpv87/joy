{-# LANGUAGE DeriveAnyClass #-}

module Main where

import Control.Monad
import Data.Maybe (fromMaybe)

data Lit
    = LBool Bool
    | LStr String
    | LNum Integer
    deriving (Show)

data JSON a
    = JNull
    | JVal a
    | JObj [(String, JSON a)]
    | JArr [JSON a]
    deriving (Show, Functor)

instance Applicative JSON where
    pure = JVal
    (<*>) = ap

instance Monad JSON where
    (>>=) :: JSON a -> (a -> JSON b) -> JSON b
    JNull >>= _ = JNull
    JVal a >>= f = f a
    JObj xs >>= f = JObj (fmap (>>= f) <$> xs)
    JArr xs >>= f = JArr (fmap (>>= f) xs)

mkBool = JVal . LBool
mkStr = JVal . LStr
mkNum = JVal . LNum

type Year = Integer
data Month = Jan | Feb | Mar | Apr | May | Jun | Jul | Aug | Sep | Oct | Nov | Dec
    deriving (Show, Eq, Enum, Bounded)

data DayOfWeek = Sun | Mon | Tue | Wed | Thu | Fri | Sat
    deriving (Show, Eq, Enum, Bounded)
type DayOfMonth = Integer
type Week = [(DayOfMonth, DayOfWeek)]

isLeapYear :: Integral a => a -> Bool
isLeapYear year
    | (year `mod` 4 /= 0) || (year `mod` 400 == 0) = False
    | otherwise = True

year :: Integer -> JSON Year
year = pure

toMonth :: Year -> JSON Month
toMonth y = JObj [(show y, JArr (JVal <$> [minBound .. maxBound]))]

toWeek :: Year -> Month -> JSON Week
toWeek year month =
    JObj [(show month, JArr (JVal <$> breakIntoWeek dom))]
  where
    ndays
        | month `elem` [Jan, Mar, May, Jul, Aug, Oct, Dec] = 31
        | month `elem` [Apr, Jun, Sep, Nov] = 30
        | isLeapYear year = 29
        | otherwise = 28
    firstDow = dayOfWeek year month 1
    dow = dropWhile (/= firstDow) . concat . repeat $ [minBound .. maxBound]
    dom = zip [1 .. ndays] dow
    breakIntoWeek :: [(DayOfMonth, DayOfWeek)] -> [Week]
    breakIntoWeek [] = []
    breakIntoWeek (d : ds) =
        let (w, ws) = break ((== Sun) . snd) ds
         in (d : w) : breakIntoWeek ws

main = do
    print $ do
        x <- JArr (fmap year [2000 .. 2020])
        y <- toMonth x
        z <- toWeek x y
        pure z

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
