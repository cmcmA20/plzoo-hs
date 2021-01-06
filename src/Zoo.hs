-- This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo where

import           Control.Algebra
import qualified Control.Concurrent              as S
import           Control.Effect.Exception
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Lens
import           Control.Monad (forever, forM_, void, when)
import           Data.Generics.Labels ()
import           Data.Kind (Type)
import           Data.Text (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           GHC.Generics
import qualified GHC.IO.Exception                as S
import           Options.Applicative
import           Prelude hiding (readFile)
import qualified System.Environment              as S
import qualified System.Exit                     as S
import qualified System.Info                     as S
import qualified System.IO                       as S
import qualified System.Posix.Process            as S
import qualified System.Posix.Signals            as S

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

newtype InternalError = MkInternalError { unInternalError :: Text }

showIE :: InternalError -> Text
showIE (MkInternalError t) = "Internal error: " <> t

instance Show InternalError where
  show = T.unpack . showIE

instance Exception InternalError

raiseInternalError :: Has (Lift IO) sig m => Text -> m a
raiseInternalError = throwIO . MkInternalError

maybeRaiseInternalError :: Has (Lift IO) sig m => Text -> Maybe a -> m a
maybeRaiseInternalError r Nothing  = throwIO $ MkInternalError r
maybeRaiseInternalError _ (Just v) = pure v

data ErrorKind
  = EKSyntax
  | EKType
  | EKCompile
  | EKRuntime

showErrorKind :: ErrorKind -> Text
showErrorKind EKSyntax    = "Syntax error"
showErrorKind EKType      = "Type error"
showErrorKind EKCompile   = "Compilation error"
showErrorKind EKRuntime   = "Runtime error"

instance Show ErrorKind where
  show = T.unpack . showErrorKind

data PLZException = MkPLZException
  { loc :: !Location
  , ek  :: !ErrorKind
  , msg :: !Text }

showPLZE :: PLZException -> Text
showPLZE MkPLZException{..} = showErrorKind ek <>
  case loc of
    LNowhere      -> ": " <> msg
    LLocation _ _ -> " at " <> showLocation loc <> ": " <> msg

instance Show PLZException where
  show = T.unpack . showPLZE

instance Exception PLZException

raiseError :: Has (Lift IO) sig m => ErrorKind -> Location -> Text -> m a
raiseError ek loc msg = throwIO $ MkPLZException loc ek msg

-- TODO refactor all these uncertain error throws
maybeRaiseError :: Has (Lift IO) sig m => ErrorKind -> Location -> Text -> Maybe a -> m a
maybeRaiseError ek loc msg Nothing  = raiseError ek loc msg
maybeRaiseError _  _   _   (Just v) = pure v

raiseErrorClassic :: ErrorKind -> Location -> Text -> a
raiseErrorClassic ek loc msg = throw $ MkPLZException loc ek msg

printError :: Has (Lift IO) sig m => PLZException -> m ()
printError = sendIO . TIO.putStrLn . showPLZE

showWithParens :: Integer -> Integer -> (a -> Text) -> (a -> Text)
showWithParens maxLevel atLevel p =
  if maxLevel < atLevel
     then \x -> "(" <> p x <> ")"
     else p

data Opts = MkOpts
  { wrappers       :: !(Maybe [Text])
  , noWrapper      :: !Bool
  , nonInteractive :: !Bool
  , onlyLangInfo   :: !Bool
  , fileToLoad     :: ![Text] }
  deriving (Generic, Show)

parseOpts :: Parser Opts
parseOpts = MkOpts
  <$> optional (many (strOption ( long "wrapper"
    <> metavar "WRAPPER"
    <> help "Specify a command-line wrapper to be used (such as rlwrap or ledit)" )))
  <*> switch (long "no-wrapper"
    <> help "Do not use a command-line wrapper")
  <*> switch (short 'n'
    <> help "Do not run the interactive toplevel")
  <*> switch (short 'v'
    <> help "Print language information and exit")
  <*> many (strOption (short 'l'
    <> help "Load file into the initial environment"))

applyOpts :: forall env cmd sig m. Language env cmd sig m => m ()
applyOpts = do
  o <- sendIO $ execParser fullOpts
  when (o ^. #onlyLangInfo) do
    ln <- asks @(LangStatic env cmd) name
    sendIO do
      TIO.putStrLn $ ln <> " (" <> T.pack S.os <> ")"
      S.exitSuccess
  modify @(LangDynamic env) (& #wrapper %~ (o ^. #wrappers <>))
  when (o ^. #noWrapper) do
    modify @(LangDynamic env) (& #wrapper .~ Nothing)
  when (o ^. #nonInteractive) do
    modify @(LangDynamic env) (& #interactiveShell .~ False)
  where
    fullOpts :: ParserInfo Opts
    fullOpts = info (parseOpts <**> helper)
      (  fullDesc
      <> progDesc "exists s t. Lang s -> Lang t"
      <> header "The Programming Languages Zoo" )

data LangStatic (env :: Type) (cmd :: Type) = MkLangStatic
  { name           :: !Text
  , options        :: ![(Text, Text, Text)]
  , fileParser     :: !(Maybe (Text -> [cmd]))
  , toplevelParser :: !(Maybe (Text -> cmd))
  , exec           :: env -> cmd -> env
  , prettyPrinter  :: env -> Text }
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
  , Has (Lift IO) sig m
  )

-- TODO show usage depending on target language parser
-- usage
--   :: forall env cmd sig m
--   .  Language env cmd sig m
--   => m Text
-- usage = do
--   fp <- asks @(LangStatic env cmd) fileParser
--   ln <- T.pack <$> sendIO S.getProgName
--   pure $ "Usage: " <> ln <> " [option] ..." <> case fp of
--     Nothing -> ""
--     Just _  -> " [file] ..."

addFile
  :: forall env sig m
  .  Has (State (LangDynamic env)) sig m
  => Bool
  -> Text
  -> m ()
addFile interactive filename =
  modify @(LangDynamic env) (& #files %~ ( (filename, interactive) : ))

-- TODO use anonymous arguments?
anonymous
  :: forall env sig m
  .  Has (State (LangDynamic env)) sig m
  => Text
  -> m ()
anonymous t = do
  addFile @env True t
  modify @(LangDynamic env) (& #interactiveShell .~ False)

readFile
  :: forall env cmd sig m
  .  Language env cmd sig m
  => (Text -> [cmd])
  -> Text
  -> m [cmd]
readFile p filename = do
  fc <- handle dieOnIOError $ sendIO $ TIO.readFile $ T.unpack filename
  handle discardBrokenFile $ pure $ p fc
  where
    dieOnIOError :: IOError -> m Text
    dieOnIOError = raiseInternalError . T.pack . show

    discardBrokenFile :: PLZException  -> m [cmd]
    discardBrokenFile e = printError e >> pure []

readToplevel
  :: forall env cmd sig m
  .  Language env cmd sig m
  => (Text -> cmd)
  -> m cmd
readToplevel p = do
  ln <- asks @(LangStatic env cmd) name
  let
    prompt     = ln <> "> "
    promptMore = T.replicate (T.length ln) " " <> "> "
  sendIO $ TIO.putStr prompt
  inp <- sendIO $ getMultiline promptMore
  pure $ p inp
  where
    getMultiline :: Text -> IO Text
    getMultiline pm = do
      inp <- TIO.getLine
      if not (T.null inp) && T.last inp == '\\'
         then do
           TIO.putStr pm
           (T.init inp <>) <$> getMultiline pm
         else pure inp

useFile
  :: forall env cmd sig m
  .  Language env cmd sig m
  => (Text, Bool)
  -> m ()
useFile (filename, _) = do
  flp <- asks @(LangStatic env cmd) fileParser >>=
    maybeRaiseInternalError "This language can't load files."
  ex  <- asks @(LangStatic env cmd) exec
  cs  <- readFile @env @cmd flp filename
  forM_ cs \c -> do
    modify @(LangDynamic env) (& #environment %~ \e -> ex e c)

toplevel
  :: forall env cmd sig m
  .  Language env cmd sig m
  => m ()
toplevel = do
  tlp <- asks @(LangStatic env cmd) toplevelParser >>=
    maybeRaiseInternalError "This language has no toplevel."
  ex  <- asks @(LangStatic env cmd) exec
  pr  <- asks @(LangStatic env cmd) prettyPrinter
  languageName <- asks @(LangStatic env cmd) name
  sendIO $ TIO.putStrLn
    $  languageName <> " -- programming languages zoo\n"
    <> "Type " <> eofMarker <> " to exit."
  forever $ flip catches
    [ Handler handleUserInterrupt
    , Handler gracefulEOF
    , Handler printError
    ] do
        c <- readToplevel @env tlp
        modify @(LangDynamic env) (& #environment %~ \e -> ex e c)
        res <- pr <$> gets @(LangDynamic env) environment
        sendIO $ TIO.putStrLn res
  where
    eofMarker :: Text
    eofMarker = case S.os of
      "linux" -> "Ctrl-D"
      _       -> "EOF"

    handleUserInterrupt :: AsyncException -> m ()
    handleUserInterrupt UserInterrupt = sendIO $ TIO.putStrLn "Interrupted."
    handleUserInterrupt e             = throwIO e

    gracefulEOF :: IOError -> m ()
    gracefulEOF ioe =
      case S.ioe_type ioe of
        S.EOF -> sendIO S.exitSuccess
        _     -> throwIO ioe

mainPlan
  :: forall env cmd sig m
  .  Language env cmd sig m
  => m ()
mainPlan = do
  myTid <- sendIO S.myThreadId
  void $ sendIO $ S.installHandler S.keyboardSignal (S.Catch $ S.throwTo myTid UserInterrupt) Nothing
  sendIO $ S.hSetBuffering S.stdout S.NoBuffering
  applyOpts @env @cmd
  interactive <- gets @(LangDynamic env) interactiveShell
  mwr <- gets @(LangDynamic env) wrapper
  when interactive do
    flip (maybe (pure ())) mwr \wrs -> do
      myPath  <- sendIO S.getExecutablePath
      newArgs <- (++ ["--no-wrapper"]) <$> sendIO S.getArgs
      forM_ wrs \w ->
        handle @IOError (const $ pure ()) $ void $ sendIO $
          S.executeFile (T.unpack w) True (myPath : newArgs) Nothing
  fs <- reverse <$> gets @(LangDynamic env) files
  handle printError $ forM_ fs $ useFile @env @cmd
  when interactive $ toplevel @env @cmd
