{-# LANGUAGE Strict #-}

module Ray where

import Vector3 (Vector3 (Vector3), addVec, mulVecVal)

data Ray = Ray
  { getRayOrigin :: Vector3,
    getRayDir :: Vector3,
    getRayTmin :: Double,
    getRayTmax :: Double,
    getRayDepth :: Int
  }

evalRay :: Ray -> Double -> Vector3
evalRay (Ray o d _ _ _) t = addVec o (mulVecVal d t)
