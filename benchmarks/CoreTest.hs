module Main where
import qualified Data.Maybe as M
import Data.Generics.Maybe

main :: IO () 
main = return ()

fooG :: Char
fooG = fromMaybe 'a' Nothing

fooD :: Char
fooD = M.fromMaybe 'a' Nothing



