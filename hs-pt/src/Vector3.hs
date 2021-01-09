{-# LANGUAGE Strict #-}
module Vector3 where

data Vector3 = Vector3 Double Double Double

clamp :: (Ord a) => a -> a -> a -> a
clamp mn mx = max mn . min mx

getX :: Vector3 -> Double
getX (Vector3 x _ _) = x

getY :: Vector3 -> Double
getY (Vector3 _ y _) = y

getZ :: Vector3 -> Double
getZ (Vector3 _ _ z) = z

getVal :: Vector3 -> Int -> Double
getVal (Vector3 x _ _) 0 = x
getVal (Vector3 _ y _) 1 = y
getVal (Vector3 _ _ z) 2 = z

negVec :: Vector3 -> Vector3
negVec = applyUnary negate

addVec :: Vector3 -> Vector3 -> Vector3
addVec (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 + x2) (y1 + y2) (z1 + z2)

addVecVal :: Vector3 -> Double -> Vector3
addVecVal v a = applyUnary (+ a) v

addVecValFlipped :: Double -> Vector3 -> Vector3
addVecValFlipped = flip addVecVal

subVec :: Vector3 -> Vector3 -> Vector3
subVec (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 - x2) (y1 - y2) (z1 - z2)

subVecVal :: Vector3 -> Double -> Vector3
subVecVal v a = applyUnary (\x -> x - a) v

subVecValFlipped :: Double -> Vector3 -> Vector3
subVecValFlipped = flip subVecVal

mulVec :: Vector3 -> Vector3 -> Vector3
mulVec (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 * x2) (y1 * y2) (z1 * z2)

mulVecVal :: Vector3 -> Double -> Vector3
mulVecVal v a = applyUnary (* a) v

mulVecValFlipped :: Double -> Vector3 -> Vector3
mulVecValFlipped = flip mulVecVal

divVec :: Vector3 -> Vector3 -> Vector3
divVec (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (x1 / x2) (y1 / y2) (z1 / z2)

divVecVal :: Vector3 -> Double -> Vector3
divVecVal v a = applyUnary (/ a) v

divVecValFlipped :: Double -> Vector3 -> Vector3
divVecValFlipped = flip divVecVal

dotVec :: Vector3 -> Vector3 -> Double
dotVec (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

crossVec :: Vector3 -> Vector3 -> Vector3
crossVec (Vector3 x1 y1 z1) (Vector3 x2 y2 z2) = Vector3 (y1 * z2 - z1 * y2) (z1 * x2 - x1 * z2) (x1 * y2 - y1 * x2)

maxVecVal :: Vector3 -> Double
maxVecVal (Vector3 x y z) = max x (max y z)

minVecValIndex :: Vector3 -> Int
minVecValIndex (Vector3 x y z)
  | x < y && x < z = 1
  | y < z = 2
  | otherwise = 3

maxVecValIndex :: Vector3 -> Int
maxVecValIndex (Vector3 x y z)
  | x > y && x > z = 1
  | y > z = 2
  | otherwise = 3

applyUnary :: (Double -> Double) -> Vector3 -> Vector3
applyUnary f (Vector3 x y z) = Vector3 (f x) (f y) (f z)

sqrtVec :: Vector3 -> Vector3
sqrtVec = applyUnary sqrt

powVec :: Vector3 -> Double -> Vector3
powVec v e = applyUnary (** e) v

absVec :: Vector3 -> Vector3
absVec = applyUnary abs

clampVec :: Vector3 -> Double -> Double -> Vector3
clampVec v low high = applyUnary (clamp low high) v

magnitudeSq :: Vector3 -> Double
magnitudeSq (Vector3 x y z) = x * x + y * y + z * z

magnitude :: Vector3 -> Double
magnitude v = sqrt $ magnitudeSq v

normalizeVec :: Vector3 -> Vector3
normalizeVec v = divVecVal v (magnitude v)
