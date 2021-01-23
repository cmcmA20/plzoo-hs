-- | This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo.Core where

import           Control.Algebra
import           Control.Carrier.Error.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Exception
import           Control.Effect.Runtime
import           Data.Kind                    (Type)
import           Data.Text                    (Text)
import           Options.Applicative

import           Zoo.Error

type SigKind     = (Type -> Type) -> Type -> Type
type CarrierKind = Type -> Type

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
