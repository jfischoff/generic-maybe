This module is a drop in replacement for 'Maybe'. It generalizes
the functions to any types that share the same \"sum of products\" view
of 'Maybe'.

To use the module for you type, enable GHC's DeriveGeneric extension and
derive a Generic instance for your type.

```haskell
{-# LANGUAGE DeriveGeneric #-}
import GHC.Generics
 
data Result a = Success a | Fail
    deriving (Show, Generic)
```

After which you can use the functions, like your type was 'Maybe'

```
λ> fromMaybe 'a' Fail
Success 'a'
λ> fromMaybe 'a' $ Success 'b'
Success 'b'
```