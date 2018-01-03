{-# LANGUAGE TypeInType #-}
module Control.Monad.Indexed.IO where

import Control.Monad.Indexed (IxFunctor(..), IxPointed(..), IxApplicative(..), IxMonad(..))

newtype IxIO state (s1 :: state) (s2 :: state) a = IxIO { unIxIO :: (IO a) }

instance IxFunctor (IxIO state) where
 imap f = IxIO . fmap f . unIxIO

instance IxPointed (IxIO state) where
 ireturn = IxIO . pure

instance IxApplicative (IxIO state) where
 iap (IxIO f) (IxIO m) = IxIO $ f <*> m

instance IxMonad (IxIO state) where
 ibind f (IxIO m) = IxIO $ m >>= unIxIO . f
