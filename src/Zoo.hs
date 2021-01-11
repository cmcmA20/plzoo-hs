-- | This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo where

import           Control.Algebra
import           Control.Carrier.Error.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import qualified Control.Concurrent              as S
import           Control.Effect.Exception hiding (TypeError)
import           Control.Effect.Lift
import           Control.Lens
import           Control.Monad (forever, forM_, unless, void, when)
import           Control.Monad.IO.Class (liftIO, MonadIO)
import           Data.Coerce (coerce)
import           Data.Generics.Labels ()
import           Data.Kind (Type)
import           Data.Text (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO
import           GHC.Generics (Generic)
import qualified GHC.IO.Exception                as S
import           Options.Applicative
import           Prelude hiding (readFile)
import qualified System.Exit                     as S
import qualified System.Info                     as S
import qualified System.IO                       as S
import qualified System.Posix.Signals            as S

{- Location helpers -}

data Location
  = LNowhere -- No location
  | LLocation !Integer !Integer -- Delimited location (line, column)
  deriving (Generic, Show)

showLocation :: Location -> Text
showLocation LNowhere        = "unknown location"
showLocation (LLocation r c) = "line " <> T.pack (show r) <> ", column " <> T.pack (show c)

data Located (a :: Type) = MkLocated
  { content :: !a
  , loc     :: !Location }
  deriving (Generic, Show)

locate :: Maybe Location -> a -> Located a
locate Nothing  x = MkLocated x LNowhere
locate (Just l) x = MkLocated x l

{- Error handling -}

maybeThrowIO :: (Has (Lift IO) sig m, Exception e) => e -> Maybe a -> m a
maybeThrowIO err Nothing  = throwIO err
maybeThrowIO _   (Just v) = pure v

maybeThrow :: (Has (Throw e) sig m, Exception e) => e -> Maybe a -> m a
maybeThrow err Nothing  = throwError err
maybeThrow _   (Just v) = pure v

newtype InternalError = MkInternalError { unInternalError :: Text }

showIE :: InternalError -> Text
showIE (MkInternalError t) = "Internal error: " <> t

instance Show InternalError where
  show = T.unpack . showIE

instance Exception InternalError

showErr :: Text -> Location -> Text -> Text
showErr errName l m =
  let
    locText = case l of
      LNowhere      -> ""
      LLocation _ _ -> " at " <> showLocation l
  in errName <> locText <> ": " <> m

data SyntaxError
  = SELex   !Location
  | SEParse !Location

instance Show SyntaxError where
  show (SELex   lo) = T.unpack $ showErr "Syntax error" lo "lexical error"
  show (SEParse lo) = T.unpack $ showErr "Syntax error" lo "parse error"

instance Exception SyntaxError

-- dirty
alexErrorToSyntaxError :: String -> SyntaxError
alexErrorToSyntaxError (words -> ["lexical", "error", "at", "line", ls, "column", cs]) =
  SELex $ LLocation (read (init ls)) (read cs)
alexErrorToSyntaxError (words -> ["parse", "error", "at", "line", ls, "column", cs]) =
  SEParse $ LLocation (read (init ls)) (read cs)
alexErrorToSyntaxError _ = error "alexErrorToSyntaxError is a bad function"

data LangError
  = LEType    !(Located Text)
  | LECompile !(Located Text)
  | LERuntime !(Located Text)

instance Show LangError where
  show (LEType    (MkLocated m l)) = T.unpack $ showErr "Type error" l m
  show (LECompile (MkLocated m l)) = T.unpack $ showErr "Compile error" l m
  show (LERuntime (MkLocated m l)) = T.unpack $ showErr "Runtime error" l m

instance Exception LangError

printError
  :: forall e sig m
  .  ( Exception e
     , Has (Lift IO) sig m )
  => e
  -> m ()
printError = sendIO . TIO.putStrLn . T.pack . show

defaultErrorHandler
  :: forall e sig m
  .  ( Exception e
     , Has (Lift IO) sig m )
  => ErrorC e m ()
  -> m ()
defaultErrorHandler = runError @e (const $ pure ()) pure . flip catchError (printError @e)

{- Command line options parsing -}

data DefaultOpts = MkDefaultOpts
  { nonInteractive :: !Bool
  , onlyLangInfo   :: !Bool
  , filesToLoad    :: ![Text] }
  deriving (Generic, Show)

defaultOpts :: Parser DefaultOpts
defaultOpts = MkDefaultOpts
  <$> switch (short 'n'
    <> help "Do not run the interactive toplevel")
  <*> switch (short 'v'
    <> help "Print language information and exit")
  <*> many (strOption (short 'l'
    <> help "Load file into the initial environment"))

parseOpts
  :: forall clo sig m
  .  ( Has (Reader (LangOpts clo)) sig m
     , Has (Lift IO) sig m )
  => m (clo, DefaultOpts)
parseOpts = do
  po <- asks @(LangOpts clo) unLangOpts
  sendIO $ execParser $ fullOptInfo po
  where
    fullOptInfo :: Parser a -> ParserInfo (a, DefaultOpts)
    fullOptInfo p = info (liftA2 (,) p defaultOpts <**> helper)
      (  fullDesc
      <> progDesc "Too many levels of abstraction"
      <> header "The Programming Languages Zoo" )

{- Core -}

data Runtime (m :: Type -> Type) (k :: Type) where
  RRead :: Runtime m Text
  RWrite :: Text -> Runtime m ()

rRead :: Has Runtime sig m => m Text
rRead = send RRead

rWrite :: Has Runtime sig m => Text -> m ()
rWrite = send . RWrite

newtype RuntimePureC (m :: Type -> Type) (a :: Type) = MkRuntimePureC { runRuntimePureC :: m a }
  deriving (Applicative, Functor, Monad)

instance Algebra sig m => Algebra (Runtime :+: sig) (RuntimePureC m) where
  alg _   (L RRead)      c = pure $ "" <$ c
  alg _   (L (RWrite _)) c = c <$ pure ()
  alg hdl (R other)      c = MkRuntimePureC $ alg (runRuntimePureC . hdl) other c

newtype RuntimeIOC (m :: Type -> Type) (a :: Type) = MkRuntimeIOC { runRuntimeIOC :: m a }
  deriving (Applicative, Functor, Monad, MonadIO)

instance (MonadIO m, Algebra sig m) => Algebra (Runtime :+: sig) (RuntimeIOC m) where
  alg _   (L RRead)      c = (<$ c) <$> liftIO TIO.getLine
  alg _   (L (RWrite t)) c = c <$ liftIO (TIO.putStr t)
  alg hdl (R other)      c = MkRuntimeIOC $ alg (runRuntimeIOC . hdl) other c


newtype LangName = MkLangName { unLangName :: Text }

newtype LangOpts (clo :: Type) =
  MkLangOpts { unLangOpts :: Parser clo }

newtype LangInit (clo :: Type) (ctx :: Type) =
  MkLangInit { unLangInit :: clo -> ctx }

newtype LangParser (cmd :: Type) =
  MkLangParser { unLangParser :: forall sig m. Has (Throw SyntaxError) sig m => Text -> m cmd }

newtype LangExec (cmd :: Type) (sem :: Type) (ctx :: Type) =
  MkLangExec { unLangExec :: forall sig m. ( Has (Throw LangError) sig m, Has (State ctx) sig m, Has Runtime sig m ) => cmd -> m sem }

newtype LangPP (sem :: Type) (ctx :: Type) =
  MkLangPP { unLangPP :: forall sig m. Has (Reader ctx) sig m => sem -> m Text }

type MetaRTS clo cmd sem ctx sig m =
  ( Has (Reader LangName) sig m
  , Has (Reader (LangOpts clo)) sig m
  , Has (Reader (LangInit clo ctx)) sig m
  , Has (Reader (Maybe (LangParser cmd))) sig m
  , Has (Reader (Maybe (LangParser [cmd]))) sig m
  , Has (Reader (LangExec cmd sem ctx)) sig m
  , Has (Reader (LangPP sem ctx)) sig m
  , Has Runtime sig m
  , Has (Lift IO) sig m )


runCommand
  :: forall cmd sem ctx sig m
  .  ( Has (Reader (LangExec cmd sem ctx)) sig m
     , Has (State ctx) sig m
     , Has Runtime sig m
     , Has (Throw LangError) sig m )
  => cmd
  -> m sem
runCommand c = do
  ex <- asks @(LangExec cmd sem ctx) unLangExec
  ex c

readToplevel
  :: forall clo cmd sem ctx sig m
  .  ( MetaRTS clo cmd sem ctx sig m
     , Has (Throw SyntaxError) sig m )
  => m cmd
readToplevel = do
  tlp' <- ask @(Maybe (LangParser cmd)) >>=
    maybeThrowIO (MkInternalError "This language has no toplevel.")
  let tlp = unLangParser tlp'
  ln <- asks @LangName unLangName
  let
    prompt     = ln <> "> "
    promptMore = T.replicate (T.length ln) " " <> "> "
  sendIO $ TIO.putStr prompt
  inp <- sendIO $ getMultiline promptMore
  if T.all (== '\n') inp
     then readToplevel @clo @cmd @sem @ctx
     else tlp inp
  where
    getMultiline :: Text -> IO Text
    getMultiline pm = do
      inp <- TIO.getLine
      if not (T.null inp) && T.last inp == '\\'
         then do
           TIO.putStr pm
           ((T.init inp <> "\n") <>) <$> getMultiline pm
         else pure $ inp <> "\n"

printResult
  :: forall sem ctx sig m
  .  ( Has (Reader (LangPP sem ctx)) sig m
     , Has (Reader ctx) sig m
     , Has (Lift IO) sig m
     , Show sem )
  => sem
  -> m ()
printResult v = do
  pp <- asks @(LangPP sem ctx) unLangPP
  rrt <- pp v
  unless (T.null rrt) $ sendIO $ TIO.putStrLn rrt

toplevel
  :: forall clo cmd sem ctx sig m
  .  ( MetaRTS clo cmd sem ctx sig m
     , Has (State ctx) sig m
     , Show sem )
  => m ()
toplevel = do
  languageName <- coerce <$> ask @LangName
  sendIO $ TIO.putStrLn
    $  languageName <> " -- programming languages zoo\n"
    <> "Type " <> eofMarker <> " to exit."
  forever $ flip catches
    [ Handler handleUserInterrupt
    , Handler gracefulEOF
    ] $ defaultErrorHandler @SyntaxError $ defaultErrorHandler @LangError do
          c <- readToplevel @clo @cmd @sem @ctx
          r <- runCommand @cmd @sem @ctx c
          curCtx <- get @ctx
          runReader curCtx $ printResult @sem @ctx r
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

readFile
  :: forall cmd sig m
  .  ( Has (Reader (Maybe (LangParser [cmd]))) sig m
     , Has (Throw SyntaxError) sig m
     , Has (Lift IO) sig m )
  => Text
  -> m [cmd]
readFile filename = do
  fp' <- ask @(Maybe (LangParser [cmd])) >>=
    maybeThrowIO (MkInternalError "This language can't load files.")
  let fp = unLangParser fp'
  fc <- handle dieOnIOError $ sendIO $ TIO.readFile $ T.unpack filename
  fp fc
  where
    dieOnIOError :: IOError -> m Text
    dieOnIOError = throwIO . MkInternalError . T.pack . show

useFile
  :: forall clo cmd sem ctx sig m
  .  ( MetaRTS clo cmd sem ctx sig m
     , Has (State ctx) sig m )
  => Text
  -> m ()
useFile filename =
  defaultErrorHandler @SyntaxError do
    cs  <- readFile @cmd filename
    defaultErrorHandler @LangError $
      forM_ cs $ runCommand @cmd @sem @ctx

zooMain
  :: forall clo cmd sem ctx sig m
  .  ( MetaRTS clo cmd sem ctx sig m
     , Show sem )
  => m ()
zooMain = do
  sendIO do
    myTid <- S.myThreadId
    void $ S.installHandler S.keyboardSignal (S.Catch $ S.throwTo myTid UserInterrupt) Nothing
    S.hSetBuffering S.stdout S.NoBuffering

  (opts, defOpts) <- parseOpts
  when (defOpts ^. #onlyLangInfo) do
    ln <- asks unLangName
    sendIO do
      TIO.putStrLn $ ln <> " (" <> T.pack S.os <> ")"
      S.exitSuccess

  initState <- asks @(LangInit clo ctx) unLangInit
  evalState (initState opts) $ forM_ (defOpts ^. #filesToLoad) $ useFile @clo @cmd @sem @ctx
  unless (defOpts ^. #nonInteractive) $
    evalState (initState opts) $ toplevel @clo @cmd @sem @ctx
