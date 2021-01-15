module Zoo.Debug where

import Control.Effect.Lift
import Control.Effect.Reader
import Control.Monad ( when )
import Text.Pretty.Simple

data Debug = MkDebug
  { lexer   :: !Bool
  , parser  :: !Bool
  , context :: !Bool }
  deriving (Eq, Ord, Read, Show)

defaultDebug :: Debug
defaultDebug = MkDebug False False False

debugPrint
  :: forall x sig m
  .  ( Has (Reader Debug) sig m
     , Has (Reader x) sig m
     , Has (Lift IO) sig m
     , Show x )
  => (Debug -> Bool)
  -> m ()
debugPrint p = do
  curCtx <- ask @x
  d <- ask @Debug
  when (p d) do
    sendIO $ pPrint curCtx
