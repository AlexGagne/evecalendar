{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Eve.Internal.Types.CachedUntil (CachedUntil, cachedUntil) where 

-- | Implementing 'CachedUntil' will let you create types that you can use 'cachedUntil' on to get their cached time
class CachedUntil a c | a -> c where
  -- | 'cachedUntil' returns the time until which the data obtained will be cached 
  cachedUntil :: a -> c