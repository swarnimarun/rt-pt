@ECHO OFF
Rem IMHO bat files should always echo
@ECHO ON
cd src
cabal install --lib vector primitive random
ghc -o ../ptrender Main.hs
cd ..

