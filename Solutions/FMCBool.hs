module ExBool where

-- Do not alter this import!
import Prelude
    ( Show(..)
    , Eq(..)
    , Ord(..)
    , Num(..)
    , Enum(..)
    , Integral(..)
    , Int
    , Char
    , (++)
    , ($)
    , (.)
    , undefined
    , error
    , otherwise
    )

-- Define evenerything that is undefined,
-- without using standard Haskell functions.
-- (Hint: recursion is your friend!)

data Bool = False | True

instance Show Bool where

    show True = "True"
    show False = "False"

instance Enum Bool where

    toEnum :: Int -> Bool
    toEnum 0 = False
    toEnum 1 = True
    toEnum _ = undefined

    fromEnum :: Bool -> Int
    fromEnum False = 0
    fromEnum True = 1


-- conjunction (AND)
(&&) :: Bool -> Bool -> Bool
True && True = True
_ && _ = False

infixr 3 &&

-- disjunction (OR)
(||) :: Bool -> Bool -> Bool
False || False = False
_ || _ = True

infixr 2 ||

-- boolean negation
not :: Bool -> Bool
not True = False
not False = True

-- NAND (Sheffer stroke)
(/|\) :: Bool -> Bool -> Bool
p /|\ q = not (p && q)

infixr 2 /|\

-- NOR (aka: Peirce arrow or Quine dagger)
(\|/) :: Bool -> Bool -> Bool
p \|/ q = not (p || q)

infixr 2 \|/

-- XOR (exclusive disjunction)
(<=/=>) :: Bool -> Bool -> Bool
p <=/=> q = (p && not q) || (not p && q)

infixr 2 <=/=>

-- if-then-else expression
ifThenElse :: Bool -> a -> a -> a
ifThenElse True x _ = x
ifThenElse False _ y = y

-- logical "implies"
(==>) :: Bool -> Bool -> Bool
True ==> False = False
_ ==> _ = True 

infixr 1 ==>

-- logical "implied by"
(<==) :: Bool -> Bool -> Bool
p <== q = q ==> p

infixl 1 <==

-- logical equivalence
(<=>) :: Bool -> Bool -> Bool
p <=> q = (p ==> q) && (p <== q)

infixr 1 <=>