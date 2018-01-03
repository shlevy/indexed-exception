{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeInType #-}
{-# LANGUAGE FlexibleInstances #-}
module Control.Monad.Indexed.Exception.IO where

import Control.Exception (Exception, try, throwIO)
import Control.Monad.Indexed.Exception (IxMonadException(..))
import Control.Monad.Indexed.IO (IxIO(..))
import Data.Kind (Type)

newtype IOExceptionStateExample a = IOExceptionStateExample a

instance IxMonadException (IxIO (IOExceptionStateExample Type)) where
  type ErrorConstraint (IxIO (IOExceptionStateExample Type)) e = Exception e
  type ErrorState (IxIO (IOExceptionStateExample Type)) ('IOExceptionStateExample e) = e
  type SetErrorState (IxIO (IOExceptionStateExample Type)) _ e = 'IOExceptionStateExample e
  attempt _ = IxIO . try . unIxIO
  fail = IxIO . throwIO
