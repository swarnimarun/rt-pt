{-# LANGUAGE Strict #-}
-- just write image to ppm file

module WriteImage where

-- Mutable stuff are still as annoying to understand in Haskell.
import Control.Monad.Primitive (PrimMonad, PrimState)
import qualified Data.Vector.Mutable as VM

import System.IO( withFile, IOMode(WriteMode), Handle, hPutStr)
import Text.Printf( printf )
import Vector3 ( clamp, Vector3(Vector3) )


fixedGamma :: Double
fixedGamma = 2.2

toByte :: Double -> Double -> Int
toByte x gamma =
    let e = 1.0 / gamma
        t = 255.0 * (x ** e)
        in truncate $ clamp t 0.0 255.0

writePPM :: Int -> Int -> VM.MVector (PrimState IO) Vector3 -> String -> IO ()
writePPM width height ls fname =
    withFile fname WriteMode $ \handle -> do
        hPutStr handle (printf "P3\n%d %d\n255\n" width height)
        writePPMLoop handle 0 (width * height) ls

writePPMLoop :: Handle -> Int -> Int -> VM.MVector (PrimState IO) Vector3 -> IO ()
writePPMLoop _ _ 0 _ = return ()
writePPMLoop handle index size ls =
    do
        _v <- VM.read ls index
        let (x, y, z) = case _v of Vector3 x y z -> (x, y, z)
        if index == size - 1
            then do
                    hPutStr handle $ printf "%d %d %d" (toByte x fixedGamma) (toByte y fixedGamma) (toByte z fixedGamma)
            else do
                    hPutStr handle $ printf "%d %d %d" (toByte x fixedGamma) (toByte y fixedGamma) (toByte z fixedGamma)
                    hPutStr handle " "
                    writePPMLoop handle (index + 1) size ls
