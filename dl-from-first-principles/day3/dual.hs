-- https://idontgetoutmuch.wordpress.com/2013/10/13/backpropogation-is-just-steepest-descent-with-automatic-differentiation-2/

module AD where

data Dual = Dual Double Double deriving (Show, Eq)

constDual :: Double -> Dual
constDual x = Dual x 0

idDual :: Double -> Dual
idDual x = Dual x 1.0

instance Num Dual where
  fromInteger :: Integer -> Dual
  fromInteger n = constDual $ fromInteger n
  (+) :: Dual -> Dual -> Dual
  (Dual x x') + (Dual y y') = Dual (x + y) (x' + y')
  (*) :: Dual -> Dual -> Dual
  (Dual x x') * (Dual y y') = Dual (x * y) (x * y' + y * x')
  negate :: Dual -> Dual
  negate (Dual x x') = Dual (negate x) (negate x')
  signum :: Dual -> Dual
  signum _ = undefined
  abs :: Dual -> Dual
  abs _ = undefined

instance Fractional Dual where
  fromRational :: Rational -> Dual
  fromRational r = constDual $ fromRational r
  recip :: Dual -> Dual
  recip (Dual x x') = Dual (recip x) (-x' / (x * x))

instance Floating Dual where
  pi = constDual pi
  exp :: Dual -> Dual
  exp (Dual x x') = Dual (exp x) (x' * exp x)
  log :: Dual -> Dual
  log (Dual x x') = Dual (log x) (x' / x)
  sqrt :: Dual -> Dual
  sqrt (Dual x x') = Dual (sqrt x) (x' / (2 * sqrt x))
  sin :: Dual -> Dual
  sin (Dual x x') = Dual (sin x) (x' * cos x)
  cos :: Dual -> Dual
  cos (Dual x x') = Dual (cos x) (x' * (-sin x))
  sinh :: Dual -> Dual
  sinh (Dual x x') = Dual (sinh x) (x' * cosh x)
  atanh :: Dual -> Dual
  atanh (Dual x x') = Dual (atanh x) (x' / (1 - x * x))

f = sqrt . (* 3) . sin