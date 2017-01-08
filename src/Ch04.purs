module Ch04 where

import Prelude
import Data.Array (length, filter, (..))
import Data.List (List(..))
import Data.Foldable (product)
import Data.Tuple (Tuple(..))
import Control.MonadZero (guard)


-- 4.4.1
isEven :: Int -> Boolean
isEven 0 = true
isEven n = not $ isEven (n-1)

-- 4.4.2
-- (Did it with List Instead of Array 'cause I really couldn't be bothered
-- with not being able to use pattern matching.)
evenCount :: List Int -> Int
evenCount Nil = 0
evenCount (Cons n ns) =
    if isEven n then 1 + evenCount ns
                else     evenCount ns

-- 4.7.1
squares :: Array Number -> Array Number
squares = map (\n -> n*n)

-- 4.7.2
removeNegatives :: Array Number -> Array Number
removeNegatives = filter (\n -> n >= 0.0)

-- 4.7.3
infixl 4 filter as <$?>
removeNegatives' :: Array Number -> Array Number
removeNegatives' = (<$?>) (\n -> n >= 0.0)

-- 4.10 (array comprehensions)
factors :: Int -> Array (Array Int)
factors n = filter (\xs -> product xs == n) $
     do i <- 1 .. n
        j <- i .. n
        pure [i, j]

factors' :: Int -> Array (Array Int)
factors' n = do
     do i <- 1 .. n
        j <- i .. n
        guard $ i * j == n
        pure [i, j]

-- 4.10.1
isPrime :: Int -> Boolean
isPrime n = 1 == length (factors n)

-- 4.10.2
cartesian :: forall a. Array a -> Array a -> Array (Array a)
cartesian as bs = do
    a <- as
    b <- bs
    pure [a, b]

cartesian' :: forall a b. Array a -> Array b -> Array (Tuple a b)
cartesian' as bs = do
    a <- as
    b <- bs
    pure $ Tuple a b

-- 4.10.3
pyTriple :: Int -> Array (Array Int)
pyTriple n = do
    a <- 1..n
    b <- 1..n
    c <- 1..n
    guard $ a*a + b*b == c*c
    pure [a,b,c]

-- 4.10.4
factorizations :: Int -> Array (Array Int)
factorizations 0 = []
factorizations 1 = [[1]]
factorizations 2 = [[1,2]]
factorizations 3 = [[1,3]]
factorizations 4 = [[1,4],[2,2]]
factorizations _ = []               -- XXX finish me

-- 4.13 tail call optimization demo
sumTo' :: Int -> Int
sumTo' n = n + sumTo' (n-1)

sumTo :: Int -> Int
sumTo n = sum n 0
    where
        sum 0 acc = acc
        sum n acc = sum (n-1) (acc+n)

