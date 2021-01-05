-- This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo where

import Control.Algebra
import Control.Effect.Error
import Control.Effect.Lift
import Control.Effect.Reader
import Control.Effect.State
import Control.Lens
import Control.Monad (forever, unless)
import Data.Generics.Labels ()
import Data.Kind (Type)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import GHC.Generics
import System.Info (os)
import qualified System.Posix.Signals as S

-- FIXME use alex type for delimited loc
data Location
  = LNowhere -- No location
  | LLocation !Integer !Integer -- Delimited location (row, column) ?

showLocation :: Location -> Text
showLocation LNowhere        = "Unknown location"
showLocation (LLocation r c) = "Row " <> T.pack (show r) <> ", Column " <> T.pack (show c)

instance Show Location where
  show = T.unpack . showLocation

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
  | EKOther !Text

showErrorKind :: ErrorKind -> Text
showErrorKind EKSyntax    = "Syntax error"
showErrorKind EKType      = "Type error"
showErrorKind EKCompile   = "Compilation error"
showErrorKind EKRuntime   = "Runtime error"
showErrorKind (EKOther t) = t <> " error"

instance Show ErrorKind where
  show = T.unpack . showErrorKind

data PLZException = MkPLZException
  { loc :: !Location
  , ek  :: !ErrorKind
  , msg :: !Text }
  deriving Show

showPLZException :: PLZException -> Text
showPLZException MkPLZException{..} = showErrorKind ek <>
  case loc of
    LNowhere      -> ": " <> msg
    LLocation _ _ -> " at " <> showLocation loc <> ": " <> msg

raiseError :: Has (Throw PLZException) sig m => ErrorKind -> Location -> Text -> m a
raiseError ek loc msg = throwError $ MkPLZException loc ek msg

showWithParens :: Integer -> Integer -> (a -> Text) -> (a -> Text)
showWithParens maxLevel atLevel p =
  if maxLevel < atLevel
     then \x -> "(" <> p x <> ")"
     else p

data LangStatic (env :: Type) (cmd :: Type) = MkLangStatic
  { name           :: !Text
  , options        :: ![(Text, Text, Text)]
  , fileParser     :: !(Maybe (Text -> [cmd]))
  , toplevelParser :: !(Maybe (Text -> cmd))
  , exec           :: env -> cmd -> env
  , printer        :: env -> Text }
  deriving Generic

data LangDynamic (env :: Type) = MkLangDynamic
  { environment      :: !env
  , interactiveShell :: !Bool
  , wrapper          :: !(Maybe [Text])
  , files            :: ![(Text, Bool)] }
  deriving Generic

type Language env cmd sig m =
  ( Has (Reader (LangStatic env cmd)) sig m
  , Has (State (LangDynamic env)) sig m
  , Has (Error PLZException) sig m
  )

usage :: forall env cmd sig m. Has (Reader (LangStatic env cmd)) sig m => m Text
usage = do
  fp <- asks @(LangStatic env cmd) fileParser
  ln <- asks @(LangStatic env cmd) name
  pure $ "Usage: " <> ln <> " [option] ..." <> case fp of
    Nothing -> ""
    Just _  -> " [file] ..."

addFile :: forall env sig m. Has (State (LangDynamic env)) sig m => Bool -> Text -> m ()
addFile interactive filename =
  modify @(LangDynamic env) (& #files %~ ( (filename, interactive) : ))

anonymous :: forall env sig m. Has (State (LangDynamic env)) sig m => Text -> m ()
anonymous str = do
  addFile @env True str
  modify @(LangDynamic env) (& #interactiveShell .~ False)

toplevel :: forall env cmd sig m. (Language env cmd sig m, Has (Lift IO) sig m) => env -> m ()
toplevel ctx = do
  let
    eof = case os of
      "linux" -> "Ctrl-D"
      _       -> "EOF"
  mtlp <- asks @(LangStatic env cmd) toplevelParser
  case mtlp of
    Nothing  ->
      raiseError (EKOther "Toplevel") LNowhere "No parser"
    Just tlp -> do
      languageName <- asks @(LangStatic env cmd) name
      sendIO $ TIO.putStrLn $ languageName <> " -- programming languages zoo"
      sendIO $ TIO.putStrLn $ "Type " <> eof <> " to exit."
      flip (catchError @PLZException) (const (pure ())) $ forever do
        pure () -- FIXME
  pure ()

mainPlan :: forall env cmd sig m. (Language env cmd sig m, Has (Lift IO) sig m) => m ()
mainPlan = do
  _ <- sendIO $ S.installHandler S.keyboardSignal (S.Catch $ pure ()) Nothing
  interactive <- gets @(LangDynamic env) interactiveShell
  unless interactive do
    pure ()
  initCtx <- gets @(LangDynamic env) environment
  toplevel @env @cmd initCtx
