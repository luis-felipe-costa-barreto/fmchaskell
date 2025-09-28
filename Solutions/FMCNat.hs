{-# LANGUAGE GADTs #-}

module ExNat where

-- Do not alter this import!
-- Para que eu não precisasse alterar esse "import", fiz outro
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Integral(..)
    , Bool(..) , not , (&&) , (||)
    , ($)
    , (.)
    , (++)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Nat where
  O :: Nat
  S :: Nat -> Nat

----------------------------------------------------------------
-- typeclass implementations
----------------------------------------------------------------

instance Show Nat where

    -- zero  should be shown as O
    -- three should be shown as SSSO
    show O = "O"
    show (S n) = "S" ++ show n


instance Eq Nat where

    (==) :: Nat -> Nat -> Bool
    O == O = True
    S _ == O = False
    O == S _ = False
    S n == S m = n == m

instance Ord Nat where

    (<=) :: Nat -> Nat -> Bool
    O <= _ = True
    S _ <= O = False
    S n <= S m = n <= m

    -- Ord does not REQUIRE defining min and max.
    -- Howevener, you should define them WITHOUT using (<=).
    -- Both are binary functions: max m n = ..., etc.

    min :: Nat -> Nat -> Nat
    min (S n) (S m) = S (min n m)
    min _ _ = O

    max :: Nat -> Nat -> Nat
    max O n = n
    max n O = n
    max (S n) (S m) = S (max n m)


----------------------------------------------------------------
-- some sugar
----------------------------------------------------------------

zero, one, two, three, four, five, six, seven, eight :: Nat
zero  = O
one   = S zero
two   = S one
three = S two
four  = S three
five  = S four
six   = S five
seven = S six
eight = S seven

----------------------------------------------------------------
-- internalized predicates
----------------------------------------------------------------

isZero :: Nat -> Bool
isZero O = True
isZero (S _) = False

-- pred is the predecessor but we define zero's to be zero
pred :: Nat -> Nat
pred O = O
pred (S n) = n 

even :: Nat -> Bool
even O = True
even (S O) = False
even (S (S n)) = even n

odd :: Nat -> Bool
odd O = False
odd (S O) = True
odd (S (S n)) = odd n

----------------------------------------------------------------
-- operations
----------------------------------------------------------------

-- addition
(<+>) :: Nat -> Nat -> Nat
n <+> O   = n
n <+> S m = S (n <+> m)

infixl 6 <+>

-- This is called the dotminus or monus operator
-- (also: proper subtraction, arithmetic subtraction, ...).
-- It behaves like subtraction, except that it returns 0
-- when "normal" subtraction would return a negative number.

(<->) :: Nat -> Nat -> Nat
n <-> O = n
O <-> _ = O
S n <-> S m = n <-> m

infixl 6 <->

monus :: Nat -> Nat -> Nat
monus = (<->)

-- multiplication
(<*>) :: Nat -> Nat -> Nat
_ <*> O = O
n <*> S m = n <*> m + n

infixl 7 <*>

times :: Nat -> Nat -> Nat
times = (<*>)

-- power / exponentiation
(<^>) :: Nat -> Nat -> Nat
_ <^> O = one
n <^> S m = n <^> m * n

infixr 8 <^>

pow :: Nat -> Nat -> Nat
pow = (<^>)

exp :: Nat -> Nat -> Nat
exp = (<^>)

-- quotient
(</>) :: Nat -> Nat -> Nat
_ </> O = undefined
n </> m =
  if m <= n then S ((n <-> m) </> m) else O

infixl 7 </>

-- remainder
(<%>) :: Nat -> Nat -> Nat
n <%> m = n <-> n </> m <*> m

-- euclidean division
-- missão
eucdiv :: (Nat, Nat) -> (Nat, Nat)
eucdiv (n, m) = (n </> m, n <%> m)

-- divides
(<|>) :: Nat -> Nat -> Bool
n <|> m = n <%> m == O

divides :: Nat -> Nat -> Bool
divides = (<|>)

-- distance between nats
-- x `dist` y = |x - y|
-- (Careful here: this - is the real minus operator!)
(|-|) :: Nat -> Nat -> Nat
n |-| m =
  if m <= n then n <-> m else m <-> n

dist :: Nat -> Nat -> Nat
dist = (|-|)

factorial :: Nat -> Nat
factorial O = S O
factorial (S n) = S n * factorial n

-- signum of a number (-1, 0, or 1)
sg :: Nat -> Nat
sg O = O
sg (S _) = S O

-- lo b a is the floor of the logarithm base b of a
lo :: Nat -> Nat -> Nat
lo O _ = undefined
lo _ O = undefined
lo (S O) _ = undefined
lo _ (S O) = O
lo n m = S (lo n (m </> n))

infixr 8 `lo`

----------------------------------------------------------------
-- Num & Integral fun
----------------------------------------------------------------

-- For the following functions we need Num(..).
-- Do NOT use the following functions in the definitions above!

toNat :: Integral a => a -> Nat
toNat 0 = O
toNat x = S (toNat (x - 1))

fromNat :: Integral a => Nat -> a
fromNat O = 0
fromNat (S n) = fromNat n + 1


-- Voilá: we can now easily make Nat an instance of Num.
instance Num Nat where

    (+) = (<+>)
    (*) = (<*>)
    (-) = (<->)
    abs n = n
    signum = sg
    fromInteger x
      | x < 0     = undefined
      | x == 0    = undefined
      | otherwise = undefined