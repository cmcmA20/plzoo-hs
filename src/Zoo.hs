-- This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo where

import Control.Algebra
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.State
import Data.Kind (Type)
import Data.Text (Text)
import qualified System.Posix.Signals as S

-- FIXME use alex type for delimited loc
data Location
  = LNowhere -- No location
  | LLocation Text Text -- Delimited location
  deriving Show

data Located (a :: Type) = MkLocated
  { content :: !a
  , loc     :: !Location }

locate :: Maybe Location -> a -> Located a
locate Nothing    x = MkLocated x LNowhere
locate (Just loc) x = MkLocated x loc

data ErrorKind
  = EKSyntax
  | EKType
  | EKCompile
  | EKRuntime
  | EKUnknown !Text
  deriving Show

data PLZError = MkPLZError
  { loc :: !Location
  , ek  :: !ErrorKind
  , msg :: !Text }
  deriving Show

-- raiseError :: ErrorKind -> Location -> a
-- raiseError ek loc = throw MkPLZError {msg = "", ..}

data LangStatic (env :: Type) (cmd :: Type) = MkLangStatic
  { name :: !Text
  , options :: ![(Text, Text, Text)]
  , initialEnvironment :: !env
  , fileParser :: !(Maybe (Text -> [cmd]))
  , toplevelParser :: !(Maybe (Text -> cmd))
  , exec :: env -> cmd -> env
  }

data LangDynamic (env :: Type) = MkLangDynamic
  { environment :: !env
  , interactiveShell :: !Bool
  , wrapper :: !(Maybe [Text])
  , files :: ![(Text, Bool)]
  }

type Language env cmd sig m =
  ( Has (Reader (LangStatic env cmd)) sig m
  , Has (State (LangDynamic env)) sig m
  )

usage :: forall env cmd sig m. Has (Reader (LangStatic env cmd)) sig m => m Text
usage = do
  fp <- asks @(LangStatic env cmd) fileParser
  ln <- asks @(LangStatic env cmd) name
  pure $ "Usage: " <> ln <> " [option] ..." <> case fp of
    Nothing -> ""
    Just _  -> " [file] ..."

mainPlan :: forall env cmd sig m. (Language env cmd sig m, Has (Lift IO) sig m) => m ()
mainPlan = do
  sendIO $ S.installHandler S.keyboardSignal (S.Catch $ pure ()) Nothing
  x <- gets @(LangDynamic env) interactiveShell
  languageName <- asks @(LangStatic env cmd) name
  sendIO $ print languageName
  pure ()
