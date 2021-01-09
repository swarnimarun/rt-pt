!/bin/bash

# ensure all the deps are installed
cabal install --lib random primitive vector
cd src && ghc Main.hs -o ../ptrenderer
cd ..
