module Control.Effect.Runtime
  ( Runtime(..)
  , rExit
  , rRead
  , rWrite
  ) where

import           Control.Algebra
import           Data.Kind       (Type)
import           Data.Text       (Text)

type CarrierKind = Type -> Type

type Runtime :: CarrierKind -> Type -> Type
data Runtime m k where
  RExit  :: Runtime m ()
  RRead  :: Runtime m Text
  RWrite :: Text -> Runtime m ()

rExit :: Has Runtime sig m => m ()
rExit = send RExit

rRead :: Has Runtime sig m => m Text
rRead = send RRead

rWrite :: Has Runtime sig m => Text -> m ()
rWrite = send . RWrite
