{- | This module is a drop in replacement for 'Data.Maybe.Maybe'. It generalizes
 the functions to any types that share the same \"sum of products\" view
 of 'Data.Maybe.Maybe'.
 
 To use the module for your type, enable GHC's DeriveGeneric extension and
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
{-# LANGUAGE TypeOperators          #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE ScopedTypeVariables    #-}
module Data.Generics.Maybe 
   ( -- * Data.Maybe Functions
     fromMaybe
   , maybe
   , isJust
   , isNothing
   , fromJust
   , listToMaybe
   , maybeToList
   , catMaybes
   , mapMaybe
     -- * Convert between Maybelikes
   , convert
   -- * Exported for groking, but not for implementing.
   , MaybeLike
   ) where
import GHC.Generics
    ( Generic(..)
    , U1(..)
    , K1(K1)
    , M1(..)
    , (:+:)(..)
    , Rec0
    , C1
    , S1 
    )
import Prelude hiding (maybe)

-- DocTest setup

-- $setup
-- >>> :set -XDeriveGeneric
-- >>> data Nat = Zero | Succ Nat deriving (Show, Generic)
-- >>> data Result a = Success a | Fail deriving (Show, Generic)

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
fromMaybe :: (Generic maybe, MaybeLike (Rep maybe) a)
          => a -> maybe -> a
fromMaybe x = fromMaybe' x . toGSimple 

fromMaybe' :: a 
           -> (U1 :+: Rec0 a) b
           -> a
fromMaybe' def e = case e of 
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
maybe :: (Generic maybe, MaybeLike (Rep maybe) a)
      => b -> (a -> b) -> maybe -> b
maybe def f = maybe' def f . toGSimple
      
maybe' :: b -> (a -> b) -> (U1 :+: Rec0 a) x -> b
maybe' def f e = case e of
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
isJust :: forall maybe a. 
          (Generic maybe, MaybeLike (Rep maybe) a)
       => maybe -> Bool
isJust = isJust' . (toGSimple :: maybe -> (U1 :+: Rec0 a) p)

isJust' :: (U1 :+: Rec0 a) b -> Bool
isJust' e = case e of
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
isNothing :: forall maybe a.
             (Generic maybe, MaybeLike (Rep maybe) a)
          => maybe -> Bool   
isNothing = isNothing' . (toGSimple :: maybe -> (U1 :+: Rec0 a) p)
   
isNothing' :: (U1 :+: Rec0 a) b -> Bool
isNothing' e = case e of
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
fromJust :: (Generic maybe, MaybeLike (Rep maybe) a)
         => maybe -> a   
fromJust = fromJust' . toGSimple
   
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
listToMaybe :: (Generic maybe, MaybeLike (Rep maybe) a)
            => [a] -> maybe
listToMaybe = fromGSimple . listToMaybe' 
   
listToMaybe' :: [a] -> (U1 :+: Rec0 a) b
listToMaybe' e = case e of
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
maybeToList :: (Generic maybe, MaybeLike (Rep maybe) a)
            => maybe -> [a]
maybeToList = maybeToList' . toGSimple

maybeToList' :: (U1 :+: Rec0 a) b -> [a]
maybeToList' e = case e of
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
catMaybes :: (Generic maybe, MaybeLike (Rep maybe) a)
          => [maybe] -> [a]
catMaybes = catMaybes' . map toGSimple 

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
mapMaybe :: (Generic maybe, MaybeLike (Rep maybe) b)
         => (a -> maybe) -> [a] -> [b]
mapMaybe f = mapMaybe' (toGSimple . f) 

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
-- | Convert between two Maybelikes
--
-- >>> convert (Just 'a') :: Result Char
-- Success 'a'
-- 
-- >>> convert (Success 'a') :: Maybe Char
-- Just 'a'
-- 
-- >>> convert (Success Zero) :: Nat
-- Succ Zero
convert :: (Generic maybe1, MaybeLike (Rep maybe1) a, Generic maybe2, MaybeLike (Rep maybe2) a)
        => maybe1 -> maybe2
convert = to . fromMaybelike . toMaybelike . from

-------------------------------------------------------------------------------
--                               Utils
-------------------------------------------------------------------------------

commuteInto :: (M1 m a (f :+: g)) p -> (M1 m a (g :+: f)) p
commuteInto x = M1 $ commuteSum $ unM1 x
{-# INLINE commuteInto #-}

commuteSum :: (f :+: g) p -> (g :+: f) p
commuteSum e = case e of
   L1 x -> R1 x
   R1 x -> L1 x
{-# INLINE commuteSum #-}

toClean :: M1 t t1 (M1 t2 t3 f :+: M1 t4 t5 (M1 t6 t7 (K1 t8 c))) p
        -> (:+:) f (K1 i c) p
toClean (M1 x) = case x of
            L1 (M1 l)           -> L1 l
            R1 (M1 (M1 (K1 r))) -> R1 $ K1 r
{-# INLINE toClean #-}
            
fromClean :: (:+:) f (K1 t c4) p
          -> M1 i c (M1 i1 c1 f :+: M1 i2 c2 (M1 i3 c3 (K1 i4 c4))) p
fromClean e = case e of
            L1 l      -> M1 $ L1 $ M1 l
            R1 (K1 r) -> M1 $ R1 $ M1 $ M1 $ K1 r
{-# INLINE fromClean #-}
      
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
class MaybeLike rep any | rep -> any where
  toMaybelike   :: rep p -> (U1 :+: Rec0 any) p
  fromMaybelike :: (U1 :+: Rec0 any) p -> rep p

instance MaybeLike (M1 m a (C1 y U1 :+: C1 b (S1 e (K1 k any)))) any  where
  toMaybelike   = toClean
  {-# INLINE toMaybelike #-}
--  {-# SPECIALIZE toMaybelike :: (M1 m a (C1 y U1 :+: C1 b (S1 e (K1 k any)))) p -> (U1 :+: Rec0 any) p #-}
  fromMaybelike = fromClean
  {-# INLINE fromMaybelike #-}
--  {-# SPECIALIZE fromMaybelike :: (U1 :+: Rec0 any) p -> (M1 m a (C1 y U1 :+: C1 b (S1 e (K1 k any)))) p #-}

instance MaybeLike (M1 m a (C1 b (S1 e (K1 k any)) :+: C1 y U1)) any where
  toMaybelike   = toClean . commuteInto
  {-# INLINE toMaybelike #-}
--  {-# SPECIALIZE toMaybelike :: (M1 m a (C1 b (S1 e (K1 k any)) :+: C1 y U1)) p -> (U1 :+: Rec0 any) p #-}
  fromMaybelike = commuteInto . fromClean
  {-# INLINE fromMaybelike #-}
--  {-# SPECIALIZE fromMaybelike :: (U1 :+: Rec0 any) p ->(M1 m a (C1 b (S1 e (K1 k any)) :+: C1 y U1)) p #-}

toGSimple :: (Generic maybe, MaybeLike (Rep maybe) a)
          => maybe -> (U1 :+: Rec0 a) p
toGSimple = toMaybelike . from
{-# INLINE toGSimple #-}

fromGSimple :: (Generic maybe, MaybeLike (Rep maybe) a)
            => (U1 :+: Rec0 a) p -> maybe
fromGSimple = to . fromMaybelike
{-# INLINE fromGSimple #-}

