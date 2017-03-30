{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE FunctionalDependencies #-}

module Eve.Internal.Types.CachedUntil (CachedUntil, cachedUntil) where 

class CachedUntil a c | a -> c where
  cachedUntil :: a -> c