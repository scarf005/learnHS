{-# LANGUAGE InstanceSigs #-}

module Vec2d (Vec2d) where

newtype Vec2d a = Vec2d (a, a)
  deriving (Eq, Show)

zero :: Num a => Vec2d a
zero = Vec2d (0, 0)

unit :: Num a => Vec2d a
unit = Vec2d (1, 1)

instance Num a => Num (Vec2d a) where
  (+) :: Num a => Vec2d a -> Vec2d a -> Vec2d a
  Vec2d (x1, y1) + Vec2d (x2, y2) = Vec2d (x1 + x2, y1 + y2)

  (*) :: Num a => Vec2d a -> Vec2d a -> Vec2d a
  Vec2d (x1, y1) * Vec2d (x2, y2) = Vec2d (x1 * x2, y1 * y2)

  negate :: Num a => Vec2d a -> Vec2d a
  negate (Vec2d (x, y)) = Vec2d (negate x, negate y)

  abs :: Num a => Vec2d a -> Vec2d a
  abs (Vec2d (x, y)) = Vec2d (abs x, abs y)

  signum :: Num a => Vec2d a -> Vec2d a
  signum (Vec2d (x, y)) = Vec2d (signum x, signum y)

  fromInteger :: Num a => Integer -> Vec2d a
  fromInteger n = Vec2d (fromInteger n, fromInteger n)
