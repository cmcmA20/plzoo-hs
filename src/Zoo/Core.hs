-- | This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo.Core where

import           Control.Algebra
import           Control.Carrier.Error.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Exception
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Kind (Type)
import           Data.Text (Text)
import qualified Data.Text.IO                    as TIO
import           Options.Applicative
import qualified System.Exit                     as S

import Zoo.Error

type SigKind     = (Type -> Type) -> Type -> Type
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

newtype RuntimePureC (m :: CarrierKind) (a :: Type) = MkRuntimePureC { runRuntimePureC :: m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Runtime :+: sig) (RuntimePureC m) where
  alg _   (L RExit)      c = c <$ pure () -- FIXME is it right?
  alg _   (L RRead)      c = pure $ "" <$ c
  alg _   (L (RWrite _)) c = c <$ pure ()
  alg hdl (R other)      c = MkRuntimePureC $ alg (runRuntimePureC . hdl) other c

newtype RuntimeIOC (m :: CarrierKind) (a :: Type) = MkRuntimeIOC { runRuntimeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Runtime :+: sig) (RuntimeIOC m) where
  alg _   (L RExit)      c = c <$ liftIO S.exitSuccess
  alg _   (L RRead)      c = (<$ c) <$> liftIO TIO.getLine
  alg _   (L (RWrite t)) c = c <$ liftIO (TIO.putStr t)
  alg hdl (R other)      c = MkRuntimeIOC $ alg (runRuntimeIOC . hdl) other c


newtype LangName = MkLangName { unLangName
  :: Text }

newtype LangOpts (clo :: Type) = MkLangOpts { unLangOpts
  :: Parser clo }

newtype LangInit (clo :: Type) (ctx :: Type) = MkLangInit { unLangInit
  :: clo -> ctx }

newtype LangParser (cmd :: Type) = MkLangParser { unLangParser
  :: forall (sig :: SigKind) (m :: CarrierKind)
  .  Has (Throw SyntaxError) sig m
  => Text -> m cmd }

newtype LangExec (cmd :: Type) (sem :: Type) (ctx :: Type) = MkLangExec { unLangExec
  :: forall (sig :: SigKind) (m :: CarrierKind)
  .  ( Has (Throw LangError) sig m
     , Has (State ctx) sig m
     , Has Runtime sig m )
  => cmd -> m sem }

newtype LangPP (sem :: Type) (ctx :: Type) = MkLangPP { unLangPP
  :: forall (sig :: SigKind) (m :: CarrierKind)
  .  Has (Reader ctx) sig m
  => sem -> m Text }

type MetaRTS (clo :: Type) (cmd :: Type) (sem :: Type) (ctx :: Type) (sig :: SigKind) (m :: CarrierKind) =
  ( Has (Reader LangName) sig m
  , Has (Reader (LangOpts clo)) sig m
  , Has (Reader (LangInit clo ctx)) sig m
  , Has (Reader (Maybe (LangParser cmd))) sig m
  , Has (Reader (Maybe (LangParser [cmd]))) sig m
  , Has (Reader (LangExec cmd sem ctx)) sig m
  , Has (Reader (LangPP sem ctx)) sig m
  , Has Runtime sig m
  , Has (Lift IO) sig m )
