{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
module Main where
import Test.Tasty
import Test.Tasty.TH
import Test.Tasty.HUnit
import Test.HUnit
import Generics.Maybe
import Prelude hiding (maybe)
import GHC.Generics
import Control.Exception
import Control.Monad
import Control.DeepSeq

isLeft x = case x of { Left {} -> True; Right {} -> True }

main :: IO ()
main = $(defaultMainGenerator)

data Nat = Zero | Succ Nat deriving (Eq, Show, Generic)

instance NFData Nat where
   rnf x = case x of
      Zero    -> Zero `seq` ()
      Succ x  -> x `deepseq` ()

data Result a = Success a | Fail deriving (Eq, Show, Generic)


case_fromMaybe_Nothing = fromMaybe 'a' Nothing @?= 'a'

case_fromMaybe_Just = fromMaybe 'a' (Just 'b') @?= 'b'

case_fromMaybe_Fail = fromMaybe 'a' Fail @?= 'a'

case_fromMaybe_Success =  fromMaybe 'a' (Success 'b') @?= 'b'

case_fromMaybe_Zero = fromMaybe Zero Zero @?= Zero

case_fromMaybe_Succ = fromMaybe Zero (Succ (Succ Zero)) @?= Succ Zero


case_maybe_Nothing = maybe (1 :: Int) (+1) Nothing @?= 1

case_maybe_Just = maybe 1 (+1) (Just 1) @?= 2

case_maybe_Fail = maybe (1 :: Int) (+1) Fail @?= 1

case_maybe_Success = maybe 1 (+1) (Success 1) @?= 2

case_maybe_Zero = maybe (Succ Zero) Succ Zero @?= Succ Zero

case_maybe_Succ = maybe (Succ Zero) Succ (Succ (Succ Zero)) 
              @?= Succ (Succ Zero)

case_isJust_Nothing = isJust Nothing @?= False

case_isJust_Just = isJust (Just 'a') @?= True

case_isJust_Fail = isJust Fail @?= False
 
case_isJust_Successs = isJust (Success 'a') @?= True

case_isJust_Zero = isJust Zero @?= False

case_isJust_Succ = isJust (Succ Zero) @?= True

case_isNothing_Nothing = isNothing Nothing @?= True

case_isNothing_Just = isNothing (Just 'a') @?= False

case_isNothing_Fail = isNothing Fail @?= True

case_isNothing_Success = isNothing (Success 'a') @?= False
 
case_isNothing_Zero = isNothing Zero @?= True

case_isNothing_Succ = isNothing (Succ Zero) @?= False

assertException :: (NFData a, Show a) => a -> IO ()
assertException = assertBool "Expected exception: " 
                . (isLeft :: Either SomeException a -> Bool)  
                <=< try 
                . evaluate
                . force

case_fromJust_Nothing = assertException (fromJust Nothing :: Char)

case_fromJust_Just = fromJust (Just 'a') @?= 'a'

case_fromJust_Fail = assertException (fromJust Fail :: Char)

case_fromJust_Success = fromJust (Success 'a') @?= 'a'

case_fromJust_Zero = assertException (fromJust Zero :: Nat)

case_fromJust_Succ = fromJust (Succ Zero) @?= Zero

case_listToMaybe_NonEmpty_Maybe = listToMaybe ['a', 'b'] @?= Just 'a'

case_listToMaybe_Empty_Maybe = listToMaybe ([] :: [Char]) @?= Nothing

case_listToMaybe_NonEmpty_Result = listToMaybe ['a', 'b'] @?= Success 'a'

case_listToMaybe_Empty_Result = listToMaybe ([] :: [Char]) @?= Fail

case_listToMaybe_NonEmpty_Nat = listToMaybe [Zero, Succ Zero] @?= Succ Zero

case_listToMaybe_Empty_Nat = listToMaybe ([] :: [Nat]) @?= Zero

case_maybeToList_Just = maybeToList (Just True) @?= [True]

case_maybeToList_Nothing = maybeToList Nothing @?= ([] :: [Bool])

case_maybeToList_Success = maybeToList (Success True) @?= [True]

case_maybeToList_Fail = maybeToList Fail @?= ([] :: [Bool])

case_maybeToList_Succ = maybeToList (Succ Zero) @?= [Zero]

case_maybeToList_Zero = maybeToList Zero @?= [] 

case_catMaybes_Maybe = catMaybes [Just True, Nothing, Just False] 
                   @?= [True,False]

case_catMaybes_Result = catMaybes [Success True, Fail, Success False]
                    @?= [True,False]

case_catMaybes_Nat = catMaybes [Succ Zero, Zero, Succ Zero]
                 @?= [Zero,Zero]

case_mapMaybe_Maybe 
    = mapMaybe (\x -> if x then Just "True" else Nothing) [True, False, True]
  @?= ["True","True"]

case_mapMaybe_Result
    = mapMaybe (\x -> if x then Success "True" else Fail) [True, False, True]
  @?= ["True","True"]

case_mapMaybe_Nat 
   = mapMaybe (\x -> if x then Succ Zero else Zero) [True, False, True]
 @?= [Zero,Zero]


