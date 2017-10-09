{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Data.Either.Fail
    ( Fail
    , runFail
    ) where

-- | Some libraries (annoyingly) just call 'fail' of the provided Monad for
-- indicating failure.  That works reasonably well in IO, but it's hard to use
-- with pure computations.  This is a simple wrapper of either that uses 'Left'
-- for 'fail'.
newtype Fail a = Fail {runFail :: Either String a}
    deriving (Applicative, Functor)

instance Monad Fail where
    return         = Fail . return
    (Fail x) >>= f = Fail (x >>= runFail . f)
    fail           = Fail . Left
