{- | This module is a drop in replacement for 'Data.Maybe.Maybe'. It generalizes
 the functions to any types that share the same \"sum of products\" view
 of 'Data.Maybe.Maybe'.
 
 To use the module for you type, enable GHC's DeriveGeneric extension and
 derive a Generic instance for your type.
 
 @
import GHC.Generics
      
 data Result a = Success a | Fail
   deriving (Show, Generic)

 data Nat = Zero | Succ Nat
   deriving (Show, Generic)
 @
 
 After which you can use the function, like your type was 'Data.Maybe.Maybe'
-}
{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeSynonymInstances   #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
module Generics.Maybe 
   ( MaybeLike
   , fromMaybe
   , maybe
   , isJust
   , isNothing
   , fromJust
   , listToMaybe
   , maybeToList
   , catMaybes
   , mapMaybe
   -- * Exported for groking, but not for implementing.
   , GMaybeLike
   , G
   ) where
import GHC.Generics
    ( Generic(..),
      U1(..),
      K1(K1),
      M1(..),
      type (:+:)(..),
      Rec0,
      C1,
      S1 )
import Control.Lens ( Iso', Iso, over, iso, view )
import Generics.Deriving.Lens ( generic )
import qualified Control.Lens.Iso as Iso
import Prelude hiding (maybe)

-- DocTest setup

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> data Nat = Zero | Succ Nat deriving (Show, Generic)
-- >>> data Result a = Success a | Fail deriving (Show, Generic)

-- | A constraint synonym to make the type signatures look better.
--   The 'd u m b y' type variables can be ignored.
type MaybeLike maybe a d u m b y = 
   ( Generic maybe
   , GMaybeLike (Rep maybe) (G d u m b y a)
   )

-- | A generalized version of 'Data.Maybe.fromMaybe'
--
-- > fromMaybe :: a -> Maybe a -> Maybe a
--
-- >>> fromMaybe 'a' Nothing
-- 'a'
-- 
-- >>> fromMaybe 'a' $ Just 'b'
-- 'b'
-- 
-- >>> fromMaybe 'a' Fail
-- 'a'
-- 
-- >>> fromMaybe 'a' $ Success 'b'
-- 'b'
-- 
-- >>> fromMaybe Zero Zero
-- Zero
-- 
-- >>> fromMaybe Zero $ Succ (Succ Zero)
-- Succ Zero
fromMaybe :: MaybeLike maybe a d u m b y
          => a -> maybe -> a
fromMaybe x = fromMaybe' x . view gsimple 

fromMaybe' :: a 
           -> (U1 :+: Rec0 a) b
           -> a
fromMaybe' def = \case 
   L1 U1     -> def
   R1 (K1 x) -> x 
-- | A generalized version of 'Data.Maybe.maybe' 
--
-- > maybe :: b -> (a -> b) -> Maybe a -> Maybe b
--
-- >>> maybe (1 :: Int) (+1) Nothing
-- 1
--
-- >>> maybe 1 (+1) $ Just 1
-- 2
--
-- >>> maybe (1 :: Int) (+1) Fail
-- 1
--
-- >>> maybe 1 (+1) $ Success 1
-- 2
--
-- >>> maybe (Succ Zero) Succ Zero
-- Succ Zero
--
-- >>> maybe (Succ Zero) Succ $ Succ (Succ Zero)
-- Succ (Succ Zero)
maybe :: MaybeLike maybe a d u m b' y
      => b -> (a -> b) -> maybe -> b
maybe def f = maybe' def f . view gsimple
      
maybe' :: b -> (a -> b) -> (U1 :+: Rec0 a) x -> b
maybe' def f = \case
   L1 U1     -> def
   R1 (K1 x) -> f x 

-- | A generalized version of 'Data.Maybe.isJust'
--
-- > isJust :: Maybe a -> Bool
--
-- >>> isJust Nothing
-- False
-- 
-- >>> isJust $ Just 'a'
-- True
--
-- >>> isJust Fail
-- False
-- 
-- >>> isJust $ Success 'a'
-- True
-- 
-- >>> isJust Zero
-- False
-- 
-- >>> isJust $ Succ Zero
-- True
isJust :: MaybeLike maybe a d u m b y
       => maybe -> Bool
isJust = isJust' . view gsimple

isJust' :: (U1 :+: Rec0 a) b -> Bool
isJust' = \case
   L1 {} -> False
   R1 {} -> True

-- | A generalized version of 'Data.Maybe.isNothing'
--
-- > isNothing :: Maybe a -> Bool
--
-- >>> isNothing Nothing
-- True
--
-- >>> isNothing $ Just 'a'
-- False
--
-- >>> isNothing Fail
-- True
--
-- >>> isNothing $ Success 'a'
-- False
-- 
-- >>> isNothing Zero
-- True
-- 
-- >>> isNothing $ Succ Zero
-- False
isNothing :: MaybeLike maybe a d u m b y
          => maybe -> Bool   
isNothing = isNothing' . view gsimple
   
isNothing' :: (U1 :+: Rec0 a) b -> Bool
isNothing' = \case
   L1 {} -> True
   R1 {} -> False

-- | A generalized version of 'Data.Maybe.fromJust'
--
-- > fromJust :: Maybe a -> a
--
-- >>> fromJust Nothing
-- *** Exception: Generics.fromJust. You shouldn't really use this.
-- 
-- >>> fromJust $ Just 'a'
-- 'a'
--
-- >>> fromJust Fail
-- *** Exception: Generics.fromJust. You shouldn't really use this.
-- 
-- >>> fromJust $ Success 'a'
-- 'a'
--
-- >>> fromJust Zero
-- *** Exception: Generics.fromJust. You shouldn't really use this.
--
-- >>> fromJust $ Succ Zero
-- Zero
fromJust :: MaybeLike maybe a d u m b y
         => maybe -> a   
fromJust = fromJust' . view gsimple
   
fromJust' :: (U1 :+: Rec0 a) b -> a
fromJust' (R1 (K1 x)) = x
fromJust' _ = error "Generics.fromJust. You shouldn't really use this."

-- | A generalized version of 'Data.Maybe.listToMaybe'
--
-- > listToMaybe :: [a] -> Maybe a
--
-- >>> listToMaybe ['a', 'b'] :: Maybe Char
-- Just 'a'
-- 
-- >>> listToMaybe [] :: Maybe Char
-- Nothing
--
-- >>> listToMaybe ['a', 'b'] :: Result Char
-- Success 'a'
-- 
-- >>> listToMaybe [] :: Result Char
-- Fail
--
-- >>> listToMaybe [Zero, Succ Zero] :: Nat
-- Succ Zero
--
-- >>> listToMaybe [] :: Nat
-- Zero
listToMaybe :: MaybeLike maybe a d u m b y
            => [a] -> maybe
listToMaybe = view (Iso.from gsimple) . listToMaybe' 
   
listToMaybe' :: [a] -> (U1 :+: Rec0 a) b
listToMaybe' = \case
   x:_ -> R1 $ K1 x
   []  -> L1 U1

-- | A generalized version of 'Data.Maybe.maybeToList'
--
-- > maybeToList :: Maybe a -> [a]
--
-- >>> maybeToList $ Just True
-- [True]
--
-- >>> maybeToList Nothing
-- []
--
-- >>> maybeToList $ Success True
-- [True]
--
-- >>> maybeToList Fail
-- []
-- 
-- >>> maybeToList $ Succ Zero
-- [Zero]
-- 
-- >>> maybeToList Zero
-- []
maybeToList :: MaybeLike maybe a d u m b y
            => maybe -> [a]
maybeToList = maybeToList' . view gsimple

maybeToList' :: (U1 :+: Rec0 a) b -> [a]
maybeToList' = \case
   L1 {}     -> []
   R1 (K1 x) -> [x]

-- | A generalized version of 'Data.Maybe.catMaybes'
--
-- > catMaybes :: [Maybe a] -> [a]
--
-- >>> catMaybes [Just True, Nothing, Just False]
-- [True,False]
--
-- >>> catMaybes [Success True, Fail, Success False]
-- [True,False]
--
-- >>> catMaybes [Succ Zero, Zero, Succ Zero]
-- [Zero,Zero]
catMaybes :: MaybeLike maybe a d u m b y
          => [maybe] -> [a]
catMaybes = catMaybes' . map (view gsimple) 

catMaybes' :: [(U1 :+: Rec0 a) b] -> [a]
catMaybes' xs = [x | R1 (K1 x) <- xs]

-- | A generalized version of 'Data.Maybe.mapMaybe'
--
-- > mapMaybe :: (a -> Maybe b) -> [a] -> [b]
--
-- >>> mapMaybe (\x -> if x then Just "True" else Nothing) [True, False, True]
-- ["True","True"]
--
-- >>> mapMaybe (\x -> if x then Success "True" else Fail) [True, False, True]
-- ["True","True"]
-- 
-- >>> mapMaybe (\x -> if x then Succ Zero else Zero) [True, False, True]
-- [Zero,Zero]
mapMaybe :: MaybeLike maybe a' d u m b y
         => (a -> maybe) -> [a] -> [a']
mapMaybe f = mapMaybe' (view gsimple . f) 

mapMaybe' :: (a -> (U1 :+: Rec0 b) x) -> [a] -> [b]
mapMaybe' _ []     = []
mapMaybe' f (x:xs) =
   let ys = mapMaybe' f xs in
   case f x of
     L1 {}     -> ys
     R1 (K1 y) -> y:ys
-------------------------------------------------------------------------------
--                               Utils
-------------------------------------------------------------------------------

-- | A silly type synonym to make the signatures look better.
--   None of the type variables matter except @any@.
-- Read @G@eneric maybe
type G m a y b e any = M1 m a (C1 y U1 :+: C1 b (S1 e (Rec0 any)))

m1 :: Iso (M1 i c f p) (M1 i' c' f' p') (f p) (f' p')
m1 = iso unM1 M1

commuteSum :: (f :+: g) p -> (g :+: f) p
commuteSum = \case 
   L1 x -> R1 x
   R1 x -> L1 x

-- | This type class is used to swap the order of constructors so
--   unit shows up first.
--
-- > (M1 m a (C1 b (S1 e (Rec0 any)) :+: C1 y U1)) 
--
-- will become
-- 
-- > (M1 m a (C1 y U1 :+: C1 b (S1 e (Rec0 any))))
-- 
--   and
-- 
-- > (M1 m a (C1 y U1 :+: C1 b (S1 e (Rec0 any))))
-- 
-- is unchanged
-- 
-- Thus, there are only two instances and should only be, forever
-- and always ... I think.
class GMaybeLike f g | f -> g where
   gmaybelike :: Iso' (f p) (g p)
   
instance GMaybeLike (G m a y b e any) (G m a y b e any) where
   gmaybelike = iso id id 

-- commute :+: 
instance GMaybeLike (M1 m a (C1 b (S1 e (Rec0 any)) :+: C1 y U1)) 
                    (G m a y b e any) where
   gmaybelike = iso invo invo where
      invo = over m1 commuteSum

-- Get rid of all the meta info
clean :: Iso (G m a y b e any p)
             (G m' a' y' b' e' any' p')
             ((U1 :+: Rec0 any) p)
             ((U1 :+: Rec0 any') p')
clean = iso fw bk where
   fw (M1 x) = case x of
            L1 (M1 l)      -> L1 l
            R1 (M1 (M1 r)) -> R1 r
   bk = \case
            L1 l -> M1 $ L1 $ M1 $ l
            R1 r -> M1 $ R1 $ M1 $ M1 $ r

-- Convert to the simplified generic form
gsimple :: ( Generic maybe
           , GMaybeLike (Rep maybe) (G m a y b e any)
           )
        => Iso' maybe ((U1 :+: Rec0 any) p)
gsimple = generic . gmaybelike . clean 
