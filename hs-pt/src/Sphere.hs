{-# LANGUAGE Strict #-}

module Sphere where

import Ray (Ray (Ray))
import Vector3 (Vector3 (Vector3), dotVec, subVec)

epsilonSphere :: Double
epsilonSphere = 1.0e-4

data ReflectionType = Diffuse | Specular | Refractive

data Sphere = Sphere
  { getSphereRadius :: Double,
    getSpherePos :: Vector3,
    getSphereE :: Vector3,
    getSphereF :: Vector3,
    getSphereRefType :: ReflectionType
  }

intersectSphere :: Sphere -> Ray -> (Bool, Double)
intersectSphere (Sphere r p _ _ _) (Ray o dir tmin tmax _) =
  let op = subVec p o
      dop = dotVec dir op
      d = dop * dop - dotVec op op + r * r
   in if d < 0.0
        then (False, 0.0)
        else
          let sd = sqrt d
              smin = dop - sd
           in if (tmin < smin) && (smin < tmax)
                then (True, smin)
                else
                  let smax = dop + sd
                   in if (tmin < smax) && (smax < tmax)
                        then (True, smax)
                        else (False, 0.0)
