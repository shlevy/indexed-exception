{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Control.Monad.Indexed.Exception where

import Control.Monad.Indexed (IxMonad, ibind, ireturn)
import Data.Kind (Constraint)
import Data.Proxy (Proxy(..))
import Prelude hiding (fail)

class (IxMonad m) => IxMonadException (m :: state -> state -> * -> *) where
  type ErrorConstraint m e :: Constraint
  type ErrorConstraint m e = ()
  type ErrorState m (s :: state)
  type SetErrorState m (s :: state) e :: state
  attempt :: forall proxy init s1 e2 a
           . ( ErrorConstraint m (ErrorState m s1)
             , ErrorConstraint m e2
             )
          => proxy e2
          -> m init s1 a
          -> m init (SetErrorState m s1 e2) (Either (ErrorState m s1) a)
  fail :: forall s a . ErrorConstraint m (ErrorState m s)
       => ErrorState m s -> m s s a
  -- ErrorState m (SetErrorState m s e) ~ e
  -- SetErrorState m s (ErrorState m s) ~ s
  -- attempt Proxy . ireturn = ireturn . Right
  -- attempt Proxy . fail = ireturn . Left

first :: forall m init s1 e2 a . ( IxMonadException m
                                 , ErrorConstraint m (ErrorState m s1)
                                 , ErrorConstraint m e2
                                 , ErrorState m (SetErrorState m s1 e2) ~ e2
                                 )
                                 => (ErrorState m s1 -> e2)
                                 -> m init s1 a
                                 -> m init (SetErrorState m s1 e2) a
first f m = ibind (either (fail . f) ireturn) $ attempt (Proxy @e2) m
