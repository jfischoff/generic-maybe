module Main where
import Criterion.Main
import Data.Generics.Maybe
import qualified Data.Maybe as M

main :: IO ()
main = defaultMain 
   [ bgroup "Generic" 
      [ bench "fromMaybe Nothing" $ whnf (fromMaybe ()) Nothing
      ]
   , bgroup "Data"
      [ bench "fromMaybe Nothing" $ whnf (M.fromMaybe ()) Nothing
      ] 
   ]