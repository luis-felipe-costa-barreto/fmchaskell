{-# LANGUAGE GADTs #-}

module FMCList where

import Prelude
    ( Char , String , Int , Integer , Double , Float , Bool(..)
    , Num(..) , Integral(..) , Enum(..) , Ord(..) , Eq(..)
    , not , (&&) , (||)
    , (.) , ($)
    , flip , curry , uncurry
    , otherwise , error , undefined
    )
import qualified Prelude   as P
import qualified Data.List as L
import qualified Data.Char as C
import FMCNat

{- import qualified ... as ... ?

To use a function from a qualified import
you need to prefix its name with its alias and a dot:
P.head   C.toUpper   etc.

I import these for you to test the original functions on ghci:

ghci> :t C.toUpper
C.toUpper :: Char -> Char

You MUST NOT use ANY of these in your code

-}


{- Our lists vs Haskell lists

Our definition:

data List a where
  Nil  :: List a
  Cons :: a -> List a -> List a

Here we use Haskell's built-in lists and associated syntactic sugar.
It is as if it was defined like this:

    data [a] = [] | (x : xs)

or like this:

    data [a] where
      []  :: [a]
      (:) :: a -> [a] -> [a]

write [a]       for our List a
write []        for our List
write []        for our Nil
write (x : xs)  for our Cons x xs
write [u,v]     for our u `Cons` (v `Cons` Nil)

-}

head :: [a] -> a
head [] = undefined
head (x : _) = x

tail :: [a] -> [a]
tail [] = undefined
tail (_ : xs) = xs

null :: [a] -> Bool
null [] = True
null _ = False

length :: [a] -> Nat
length [] = 0
length (_ : xs) = length xs + 1

sum :: Num a => [a] -> a
sum [] = 0
sum (x : xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x : xs) = x * product xs

-- (snoc is cons written backwards)
snoc :: a -> [a] -> [a]
snoc x [] = [x]
snoc xout (xin : xs) = xin : snoc xout xs

(<:) :: [a] -> a -> [a]
(<:) = flip snoc

reverse :: [a] -> [a]
reverse [] = []
reverse (x : xs) = snoc x (reverse xs)

(++) :: [a] -> [a] -> [a]
[] ++ ys = ys
(x : xs) ++ ys = x : (xs ++ ys)

-- right-associative for performance!
-- (what?!)
infixr 5 ++

-- different implementation of (++)
(+++) :: [a] -> [a] -> [a]
xs +++ []     = xs
xs +++ [y]    = xs <: y
xs +++ (y:ys) = (xs +++ [y]) +++ ys

-- left-associative for performance!
-- (hmm?!)
infixl 5 +++

minimum :: Ord a => [a] -> a
minimum [] = undefined
minimum [x] = x
minimum [u, v] = if u <= v then u else v
minimum (x : xs) = minimum (x : [minimum xs])

maximum :: Ord a => [a] -> a
maximum [] = undefined
maximum [x] = x
maximum [u, v] = if u >= v then u else v
maximum (x : xs) = maximum (x : [maximum xs])

-- take
take :: Nat -> [a] -> [a]
take 0 _ = []
take (S n) (x : xs) = x : take n xs

-- drop
drop :: Nat -> [a] -> [a]
drop 0 xs = xs
drop (S n) (_ : xs) = drop n xs

-- takeWhile
takeWhile :: (a -> Bool) -> [a] -> [a]
takeWhile b (x : xs) = if b x then x : takeWhile b xs else []

-- dropWhile
dropWhile :: (a -> Bool) -> [a] -> [a]
dropWhile b (x : xs) = if b x then dropWhile b xs else x : xs

-- tails
tails :: [a] -> [[a]]
tails [] = undefined
tails (x : xs) = if null xs then [[]] else xs : tails xs

-- init
init :: [a] -> [a]
init [] = undefined
init [_] = []
init (x : xs) = x : init xs

-- inits
inits :: [a] -> [[a]]
inits [] = undefined
inits [x] = [[]]
inits xs = snoc (init xs) (inits (init xs))

-- subsequences;
subsequences :: [a] -> [[a]]
subsequences = undefined

-- any
any :: (a -> Bool) -> [a] -> Bool
any _ [] = False
any f [x] = f x
any f (x : xs) = f x || any f xs

-- all
all :: (a -> Bool) -> [a] -> Bool
all _ [] = True
all f [x] = f x
all f (x : xs) = f x && all f xs 

-- and
and :: [Bool] -> Bool
and [x] = x
and (x : xs) = x && and xs

-- or
or :: [Bool] -> Bool
or [x] = x
or (x : xs) = x || or xs

-- concat
concat :: [[a]] -> [a]
concat [x] = x
concat (x : xs) = x ++ concat xs

-- elem using the funciton 'any' above
elem :: Eq a => a -> [a] -> Bool
elem x xs = any (== x) xs

-- elem': same as elem but elementary definition
-- (without using other functions except (==))
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' x1 (x2 : xs) = (x1 == x2) || elem' x1 xs

-- (!!)
(!!) :: [a] -> Nat -> a
(x : xs) !! n = if n >= length (x : xs) then undefined else
  case n of
    0 -> x
    S n -> xs !! n

-- filter
filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter f (x : xs) = if f x then x : filter f xs else filter f xs

-- map
map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x : xs) = f x : map f xs

-- cycle
cycle :: [a] -> [a]
cycle [] = undefined
cycle xs = xs ++ cycle xs

-- repeat
repeat :: a -> [a]
repeat x = x : repeat x

-- replicate
replicate :: Nat -> a -> [a]
replicate 0 _ = []
replicate (S n) x = x : replicate n x

-- isPrefixOf
-- isInfixOf
-- isSuffixOf

-- zip
-- zipWith

-- intercalate
-- nub

-- splitAt
-- what is the problem with the following?:
-- splitAt n xs  =  (take n xs, drop n xs)

-- break

-- lines
-- words
-- unlines
-- unwords

-- transpose

(==*) :: Eq a => [a] -> [a] -> Bool
[] ==* [] = True
(x : xs) ==* (y : ys) = x == y && xs ==* ys

last :: [a] -> a
last [] = undefined
last [x] = x
last (x : xs) = last xs

-- checks if the letters of a phrase form a palindrome (see below for examples)
--palindrome :: String -> Bool
--palindrome [] = undefined
--palindrome xs
--  | length xs < 4 = head xs == last xs
--  | head xs == last xs = palindrome (pop (tail xs))
--  | otherwise = False

palindrome :: String -> Bool
palindrome [] = undefined
palindrome xs = xs ==* reverse xs

{-

Examples of palindromes:

"Madam, I'm Adam"
"Step on no pets."
"Mr. Owl ate my metal worm."
"Was it a car or a cat I saw?"
"Doc, note I dissent.  A fast never prevents a fatness.  I diet on cod."

-}