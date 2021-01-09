{-# LANGUAGE Strict #-}

module Main where

import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as VM
import System.Environment (getArgs)
import System.Random (mkStdGen, randomRIO, setStdGen)
import Text.Printf (printf)

-- user defined
import Ray ( Ray(Ray, getRayDir, getRayDepth), evalRay )
import Sphere
import Vector3
import WriteImage ( writePPM )

uniformFloat :: IO Double
uniformFloat = randomRIO (0.0, 1.0)

-- pi :: Double
-- pi = 3.14159265358979323846

plusInf :: Double
plusInf = 1.0e20

reflectance0 :: Double -> Double -> Double
reflectance0 n1 n2 =
    let sr = (n1 - n2) / (n1 + n2)
    in sr * sr

cosineWeightedSample :: Double -> Double -> Vector3
cosineWeightedSample u1 u2 =
    let cos_theta = sqrt (1.0 - u1)
        sin_theta = sqrt u1
        phi = 2.0 * pi * u2
        in Vector3 (cos phi * sin_theta) (sin phi * sin_theta) cos_theta

schlickReflectance :: Double -> Double -> Double -> Double
schlickReflectance n1 n2 c =
    let r0 = reflectance0 n1 n2
    in r0 + (1 - r0) * c * c * c * c * c

idealSpecularReflect :: Vector3 -> Vector3 -> Vector3
idealSpecularReflect d n = subVec d (mulVecValFlipped (2.0 * dotVec n d) n)

-- hard coding as represented
idealSpecularTransmit :: Vector3 -> Vector3 -> Double -> Double -> IO (Vector3, Double)
idealSpecularTransmit d n nout nin =
  let dRe = idealSpecularReflect d n
      out_to_in = dotVec n d < 0.0
      nl = if out_to_in then n else negVec n
      nn = if out_to_in then nout / nin else nin / nout
      cos_theta = dotVec d nl
      cos2_phi = 1.0 - nn * nn * (1.0 - cos_theta * cos_theta)
   in if cos2_phi < 0.0
        then return (dRe, 1.0)
        else do
          let dTr = normalizeVec $ subVec (mulVecValFlipped nn d) (mulVecVal nl (nn * cos_theta + sqrt cos2_phi))
              c = if out_to_in then 1.0 + cos_theta else 1.0 - dotVec dTr n
              re = schlickReflectance nout nin c
              prRe = 0.25 + 0.5 * re
          u <- uniformFloat
          if u < prRe
            then return (dRe, re / prRe)
            else return (dTr, (1.0 - re) / (1.0 - prRe))

-- Scene
rIO :: Double
rIO = 1.0

rII :: Double
rII = 1.5

defaultScene :: [Sphere]
defaultScene =
  [ Sphere 1.0e5 (Vector3 100001.0 40.8 81.6) (Vector3 0.0 0.0 0.0) (Vector3 0.75 0.25 0.25) Diffuse,
    Sphere 1.0e5 (Vector3 (-99901.0) 40.8 81.6) (Vector3 0.0 0.0 0.0) (Vector3 0.25 0.25 0.75) Diffuse,
    Sphere 1.0e5 (Vector3 50.0 40.8 1.0e5) (Vector3 0.0 0.0 0.0) (Vector3 0.75 0.75 0.75) Diffuse,
    Sphere 1.0e5 (Vector3 50.0 40.8 (-99830)) (Vector3 0.0 0.0 0.0) (Vector3 0.0 0.0 0.0) Diffuse,
    Sphere 1.0e5 (Vector3 50.0 1.0e5 81.6) (Vector3 0.0 0.0 0.0) (Vector3 0.75 0.75 0.75) Diffuse,
    Sphere 1.0e5 (Vector3 50.0 (-99918.4) 81.6) (Vector3 0.0 0.0 0.0) (Vector3 0.75 0.75 0.75) Diffuse,
    Sphere 16.5 (Vector3 27.0 16.5 47.0) (Vector3 0.0 0.0 0.0) (Vector3 0.999 0.999 0.999) Specular,
    Sphere 16.5 (Vector3 73.0 16.5 78.0) (Vector3 0.0 0.0 0.0) (Vector3 0.999 0.999 0.999) Refractive,
    Sphere 600.0 (Vector3 50.0 681.33 81.6) (Vector3 12.0 12.0 12.0) (Vector3 0.0 0.0 0.0) Diffuse
  ]

-- Scene intersect
intersectScene :: [Sphere] -> Ray -> (Bool, Double, Int)
intersectScene scene ray =
  intersectSceneAcc scene ray 0 (False, plusInf, 0)

intersectSceneAcc :: [Sphere] -> Ray -> Int -> (Bool, Double, Int) -> (Bool, Double, Int)
intersectSceneAcc [] _ _ acc = acc
intersectSceneAcc (head : tail) (Ray o d tmin tmax depth) index (chit, ctmax, csphere) =
  let (hit, smax) = intersectSphere head (Ray o d tmin tmax depth)
      nindex = index + 1
    in if hit
        then intersectSceneAcc tail (Ray o d tmin smax depth) nindex (True, smax, index)
        else intersectSceneAcc tail (Ray o d tmin tmax depth) nindex (chit, ctmax, csphere)

-- Radiance
radiance :: [Sphere] -> Ray -> IO Vector3
radiance scene ray =
  let (hit, thit, index) = intersectScene scene ray
   in if not hit
        then return (Vector3 0.0 0.0 0.0)
        else do
          let p = evalRay ray thit
              s = scene !! index
              n = normalizeVec $ subVec p (getSpherePos s)
          (quit, nf) <-
            if getRayDepth ray > 4
              then do
                let prC = maxVecVal $ getSphereF s
                random <- uniformFloat
                if random >= prC
                  then return (True, Vector3 0.0 0.0 0.0)
                  else return (False, divVecVal (getSphereF s) prC)
              else return (False, getSphereF s)
          if quit
            then return $ getSphereE s
            else do
              (d, c) <- case getSphereRefType s of
                Refractive -> idealSpecularTransmit (getRayDir ray) n rIO rII
                Specular -> return (idealSpecularReflect (getRayDir ray) n, 1.0)
                _ -> do
                  let w = if dotVec n (getRayDir ray) < 0.0 then n else negVec n
                      u1 = if abs (getX w) > 0.1 then Vector3 0.0 1.0 0.0 else Vector3 1.0 0.0 0.0
                      u = normalizeVec $ crossVec u1 w
                      v = crossVec w u
                  random1 <- uniformFloat
                  random2 <- uniformFloat
                  let dd = cosineWeightedSample random1 random2
                  return (normalizeVec (addVec (mulVecValFlipped (getZ dd) w) (addVec (mulVecValFlipped (getX dd) u) (mulVecValFlipped (getY dd) v))), 1.0)
              let f = mulVecVal nf c
              l <- radiance scene (Ray p d epsilonSphere plusInf (getRayDepth ray + 1))
              return (addVec (getSphereE s) (mulVec f l))

-- Camera
data Camera = Camera {
    getCameraPos :: Vector3,
    getCameraCX :: Vector3,
    getCameraCY :: Vector3,
    getCameraDir :: Vector3
}

defaultCamera :: Int -> Int -> Camera
defaultCamera width height =
  let pos = Vector3 50.0 52.0 295.6
      dir = normalizeVec (Vector3 0.0 (-0.042612) (-1.0))
      fov = 0.5135
      cx = Vector3 (realToFrac width * fov / realToFrac height) 0.0 0.0
      cy = mulVecVal (normalizeVec (crossVec cx dir)) fov
   in Camera pos cx cy dir

-- Main

main :: IO ()
main =
  do
    setStdGen (mkStdGen 606418532)
    smax <- getNBSamples
    let width = 1024
        height = 768
        cam = defaultCamera width height
        scene = defaultScene
    ls <- loopMain scene cam height width smax
    writePPM width height ls "test_rend.ppm"

-- total samples count
getNBSamples :: IO Int
getNBSamples =
  do
    args <- getArgs
    case length args of
        0 -> return 1
        _ -> let first = read (head args) :: Int
                in return $ quot first 4

loopMain :: [Sphere] -> Camera -> Int -> Int -> Int -> IO (VM.MVector (PrimState IO) Vector3)
loopMain scene cam height width smax =
  do
    ls <- VM.replicate (width * height) (Vector3 0.0 0.0 0.0) -- create a vector list
    loopY scene cam 0 height width smax ls  -- loop from Y = 0 to Y <= height
    return ls

loopY :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> VM.MVector (PrimState IO) Vector3 -> IO ()
loopY scene cam y height width smax ls =
  if y == height
    then return ()
    else do
      putStr $ printf "Rendering (%d spp) %.2f\r" (smax * 4) ((100.0 * realToFrac y / realToFrac (height - 1)) :: Float)
      loopX scene cam y height 0 width smax ls  -- nest loop over X = 0 to X <= width
      loopY scene cam (y + 1) height width smax ls

loopX :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> VM.MVector (PrimState IO) Vector3 -> IO ()
loopX scene cam y height x width smax ls =
  if x == width
    then return ()
    else do
      loopSy scene cam y height x width 0 smax ls
      loopX scene cam y height (x + 1) width smax ls

loopSy :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> VM.MVector (PrimState IO) Vector3 -> IO ()
loopSy scene cam y height x width sy smax ls =
  if sy == 2
    then return ()
    else do
      loopSx scene cam y height x width sy 0 smax ls
      loopSy scene cam y height x width (sy + 1) smax ls

loopSx :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> VM.MVector (PrimState IO) Vector3 -> IO ()
loopSx scene cam y height x width sy sx smax ls =
  if sx == 2
    then return ()
    else do
      l0 <- loopS scene cam y height x width sy sx 0 smax (Vector3 0.0 0.0 0.0)
      let index = (height - 1 - y) * width + x
          l1 = mulVecVal (clampVec l0 0.0 1.0) 0.25
      l2 <- VM.read ls index
      VM.write ls index (addVec l2 l1)
      loopSx scene cam y height x width sy (sx + 1) smax ls

loopS :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Vector3 -> IO Vector3
loopS scene cam y height x width sy sx s smax l0 =
  if s == smax
    then return l0
    else do
      l1 <- doS scene cam y height x width sy sx smax l0
      loopS scene cam y height x width sy sx (s + 1) smax l1

doS :: [Sphere] -> Camera -> Int -> Int -> Int -> Int -> Int -> Int -> Int -> Vector3 -> IO Vector3
doS scene cam y height x width sy sx smax l0 =
  do
    let df x = let u = (2.0 * x) in if u < 1.0 then sqrt u - 1.0 else 1.0 - sqrt (2.0 - u)
    random1 <- uniformFloat
    random2 <- uniformFloat
    let coefx = ((realToFrac sx + 0.5 + df random1) / 2.0 + realToFrac x) / realToFrac width - 0.5
        coefy = ((realToFrac sy + 0.5 + df random2) / 2.0 + realToFrac y) / realToFrac height - 0.5
        direction = addVec (addVec (mulVecVal (getCameraCX cam) coefx) (mulVecVal (getCameraCY cam) coefy)) (getCameraDir cam)
        ndirection = normalizeVec direction
        neye = addVec (getCameraPos cam) (mulVecVal direction 130.0)
    l1 <- radiance scene (Ray neye ndirection epsilonSphere plusInf 0)
    return $ addVec l0 (divVecVal l1 (realToFrac smax))
