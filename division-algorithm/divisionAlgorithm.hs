{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Main where

import Data.Bifunctor (first)
import Data.List (partition, sortBy)
import Data.Ord (comparing)
import Numeric.Natural (Natural)

main :: IO ()
main = do
  putStrLn $ "f = " ++ pprint f
  putStrLn $ "g = " ++ pprint g
  let (q, r) = divAlgorithm f g
  putStrLn $ "f = (" ++ pprint q ++ ")(" ++ pprint g ++ ") + " ++ pprint r
  putStrLn "-------------------------"
  putStrLn $ "gcd(" ++ pprint f1 ++ ", " ++ pprint g1 ++ ") = " ++ pprint (polyGCD f1 g1)

f :: (Coefficient a) => Polynomial a
f = newPoly [1, 2, 1, 1]

g :: (Coefficient a) => Polynomial a
g = newPoly [2, 1]

h :: (Coefficient a) => Polynomial a
h = newPoly [1, 0, -2]

f1 :: (Coefficient a, Fractional a) => Polynomial a
f1 = Polynomial [Term 1 4, Term (-1) 0]

g1 :: (Coefficient a, Fractional a) => Polynomial a
g1 = Polynomial [Term 1 6, Term (-1) 0]

h1 :: (Coefficient a, Fractional a) => Polynomial a
h1 = Polynomial [Term 1 3, Term (-3) 1, Term 2 0]

type Coefficient a = (Num a, Eq a, Ord a)

type Power = Natural

data Term a = Term a Power
  deriving (Eq, Show)

newtype Polynomial a = Polynomial [Term a] deriving (Eq, Show)

class PrettyPrintable a where
  pprint :: a -> String

instance (Show a, Coefficient a) => PrettyPrintable (Term a) where
  pprint (Term 0 _) = "0"
  pprint (Term 1 1) = " + " ++ "x"
  pprint (Term 1 pow)
    | pow == 0 = " + " ++ show 1
    | otherwise = " + " ++ "x^" ++ show pow
  pprint (Term coef 1)
    | coef == 0 = []
    | coef < 0 = " - " ++ show (abs coef) ++ "x"
    | otherwise = " + " ++ show coef ++ "x"
  pprint (Term coef 0)
    | coef < 0 = " - " ++ show (abs coef)
    | coef == 0 = []
    | otherwise = " + " ++ show coef
  pprint (Term coef pow)
    | coef < 0 = " - " ++ show (abs coef) ++ "x^" ++ show pow
    | coef == 0 = []
    | otherwise = " + " ++ show coef ++ "x^" ++ show pow

instance (Show a, Num a, Ord a) => PrettyPrintable (Polynomial a) where
  pprint (Polynomial []) = "0"
  pprint (Polynomial ts) = dropPlus $ go ts
    where
      dropPlus = dropWhile (\c -> c == ' ' || c == '+')
      go [] = []
      go ((Term 0 _) : ts) = go ts
      go (t : ts) = pprint t ++ go ts

class Poly a where
  type Field a
  degree :: a -> Power
  negative :: a -> a
  eval :: a -> Field a -> Field a
  mult :: a -> a -> a
  scale :: a -> Field a -> a
  (<**>) :: a -> a -> a
  (<**>) = mult
  raise :: (Integral b) => a -> b -> a
  raise a 0 = a
  raise a n = a <**> raise a (n - 1)
  ($^) :: (Integral b) => a -> b -> a
  ($^) = raise

infixr 5 $^

instance (Coefficient a) => Poly (Term a) where
  type Field (Term a) = a
  degree (Term _ p) = p
  negative (Term c p) = Term (negate c) p
  eval (Term c p) x = c * (x ^ p)
  mult (Term c1 p1) (Term c2 p2) = Term (c1 * c2) (p1 + p2)
  scale (Term c p) scalar = Term (scalar * c) p

instance Functor Term where
  fmap f (Term c p) = Term (f c) p

instance (Coefficient a) => Poly (Polynomial a) where
  type Field (Polynomial a) = a
  degree (Polynomial []) = 0
  degree ts = let (Polynomial ts') = combine ts in maximum (fmap getP ts')
  negative (Polynomial ts) = Polynomial $ map negative ts
  eval (Polynomial []) _ = 0
  eval (Polynomial (t : ts)) x = eval t x + eval (Polynomial ts) x
  mult (Polynomial t1s) (Polynomial t2s) = combine $ Polynomial [mult t1 t2 | t1 <- t1s, t2 <- t2s]
  scale (Polynomial p) scalar = Polynomial $ map (`scale` scalar) p

instance Functor Polynomial where
  fmap f (Polynomial ts) = Polynomial $ map (fmap f) ts

newPoly :: (Coefficient a) => [a] -> Polynomial a
newPoly ps = Polynomial . reverse $ go (reverse ps) 0
  where
    go [] _ = []
    go (t : ts) n
      | t == 0 = go ts (n + 1)
      | otherwise = Term t n : go ts (n + 1)

lt :: (Coefficient a) => Polynomial a -> Term a
lt (Polynomial []) = Term 0 0
lt (Polynomial (x : _)) = x

divAlgorithm :: forall a. (Coefficient a, Fractional a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
divAlgorithm _ (Polynomial []) = error "g must be a nonzero polynomial"
divAlgorithm f g = go (Polynomial []) f
  where
    ltg :: Term a
    ltg = lt g
    go :: Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
    go q r@(Polynomial rs)
      | null rs = (q, r)
      | degree g > degree r = (q, r)
      | otherwise = go (q <++> ltrltg) (r <++> negative (ltrltg `mult` g))
      where
        ltrltg :: Polynomial a
        ltrltg = Polynomial [lt r `over` ltg]
        over :: (Coefficient a, Fractional a) => Term a -> Term a -> Term a
        (Term c1 p1) `over` (Term c2 p2) = Term (c1 / c2) (p1 - p2)

(///) :: forall a. (Coefficient a, Fractional a) => Polynomial a -> Polynomial a -> (Polynomial a, Polynomial a)
(///) = divAlgorithm

(<++>) :: (Coefficient a) => Polynomial a -> Polynomial a -> Polynomial a
(Polynomial f) <++> (Polynomial g) = combine . Polynomial $ f ++ g

instance (PrettyPrintable (Polynomial a)) => PrettyPrintable (Polynomial a, Polynomial a) where
  pprint (f, g) = pprint f ++ ", remainder " ++ pprint g

polyGCD :: (Coefficient a, Fractional a) => Polynomial a -> Polynomial a -> Polynomial a
polyGCD = go
  where
    go h (Polynomial []) = h
    go h s = let (_, rem) = divAlgorithm h s in go s rem

multipleGCD :: (Coefficient a, Fractional a) => [Polynomial a] -> Polynomial a
multipleGCD = foldl polyGCD (Polynomial [])

getQ :: (Polynomial a, Polynomial a) -> Polynomial a
getQ = fst

getR :: (Polynomial a, Polynomial a) -> Polynomial a
getR = snd

sortTerms :: Polynomial a -> Polynomial a
sortTerms (Polynomial ts) = Polynomial $ sortBy (flip (comparing getP)) ts

getC :: Term a -> a
getC (Term c _) = c

getP :: Term a -> Power
getP (Term _ p) = p

getCoefficients :: (Coefficient a) => Polynomial a -> [a]
getCoefficients (Polynomial []) = []
getCoefficients ts = go ps maxpower
  where
    (Polynomial ps) = sortTerms $ combine ts
    maxpower = degree ts
    go [] _ = []
    go (x : xs) n
      | getP x == n = getC x : go xs (n - 1)
      | otherwise = 0 : go (x : xs) (n - 1)

combine :: (Num a, Eq a) => Polynomial a -> Polynomial a
combine (Polynomial ts) = sortTerms $ Polynomial (go ts)
  where
    go [] = []
    go (t : ts) =
      let (same, rest) = partition (\x -> getP x == getP t) ts
          coeffSum = sum (map getC (t : same))
       in if coeffSum == 0
            then go rest
            else Term coeffSum (getP t) : go rest

rationalRoots :: Polynomial Integer -> [(Integer, Integer)]
rationalRoots (Polynomial []) = []
rationalRoots (Polynomial ts) =
  [ (p, q)
    | p <- factor $ abs $ getC (last ts),
      q <- factor $ abs $ getC (head ts)
  ]
  where
    factor n = [k | k <- [1 .. n], n `mod` k == 0]

rationalRootsEval :: Polynomial Integer -> [(Integer, Integer)]
rationalRootsEval poly =
  filter (\x -> abs (eval (fmap fromInteger poly) (toDecimal x)) < 0.000001) $ roots ++ fmap (first negate) roots
  where
    roots = rationalRoots poly
    toDecimal :: (Integer, Integer) -> Double
    toDecimal (n, d) = fromInteger n / fromInteger d

class Differentiable a where
  diff :: a -> a
  d :: Int -> a -> a

instance (Coefficient a) => Differentiable (Term a) where
  diff (Term _ 0) = Term 0 0
  diff (Term coef pow) = Term (coef * (fromInteger . toInteger) pow) (pow - 1)
  d n f = iterate diff f !! n

instance (Coefficient a) => Differentiable (Polynomial a) where
  diff (Polynomial ts) = Polynomial $ map diff ts
  d n f = iterate diff f !! n

class (Poly a) => Integrable a where
  integral :: a -> a
  integrate :: a -> Field a -> Field a -> Field a

instance (Coefficient a, Fractional a) => Integrable (Term a) where
  integral (Term coef pow) = Term (coef / fromIntegral (pow + 1)) (pow + 1)
  integrate term lowerBound upperBound =
    let intF = integral term
     in eval intF upperBound - eval intF lowerBound

instance (Coefficient a, Fractional a) => Integrable (Polynomial a) where
  integral (Polynomial ts) = Polynomial $ map integral ts
  integrate poly lowerBound upperBound =
    let intF = integral poly
     in eval intF upperBound - eval intF lowerBound

newtonRahpson :: forall a. (Coefficient a, Fractional a) => Polynomial a -> a -> Integer -> a
newtonRahpson f x0 iter = go x0 1
  where
    df = diff f
    newton x = x - (eval f x / eval df x)
    go x n
      | n > iter = x
      | otherwise = go (newton x) (n + 1)

bisection :: (Coefficient a, Fractional a) => Polynomial a -> a -> a -> Integer -> Maybe a
bisection p min max iter = go p min max 1
  where
    go p min max n
      | n > iter = Just m
      | f min * f max > 0 = Nothing
      | f min * f m < 0 = go p min m (n + 1)
      | otherwise = go p m max (n + 1)
      where
        f = eval p
        m = (min + max) / 2
