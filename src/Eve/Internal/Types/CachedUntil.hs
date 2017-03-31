{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

{-|
Module      : Eve.Internal.Types.CachedUntil
Description : The module contains the class needed to implement cachedUntil
Copyright   : (c) Alex Gagné, 2017
License     : MIT
Stability   : experimental
-}

module Eve.Internal.Types.CachedUntil (CachedUntil, cachedUntil) where 

-- | Implementing 'CachedUntil' will let you create types that you can use 'cachedUntil' on to get their cached time
class CachedUntil a c | a -> c where
  -- | 'cachedUntil' returns the time until which the data obtained will be cached 
  cachedUntil :: a -> c