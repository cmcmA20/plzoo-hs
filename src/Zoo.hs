-- | This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo where

import           Control.Algebra
import qualified Control.Concurrent              as S
import           Control.Effect.Error
import           Control.Effect.Exception
import           Control.Effect.Lift
import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Lens
import           Control.Monad (forever, forM_, unless, void, when)
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

printError :: (Has (Lift IO) sig m, Exception e) => e -> m ()
printError = sendIO . TIO.putStrLn . T.pack . show

-- is it even needed?
-- showWithParens :: Integer -> Integer -> (a -> Text) -> (a -> Text)
-- showWithParens maxLevel atLevel p =
--   if maxLevel < atLevel
--      then \x -> "(" <> p x <> ")"
--      else p

{- Command line options parsing -}

-- TODO parametric parser
data Opts = MkOpts
  { wrappers       :: !(Maybe [Text])
  , noWrapper      :: !Bool
  , nonInteractive :: !Bool
  , onlyLangInfo   :: !Bool
  , fileToLoad     :: ![Text] }
  deriving (Generic, Show)

defaultOpts :: Parser Opts
defaultOpts = MkOpts
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

applyOpts
  :: forall sem ctx cmd sig m
  .  Language sem ctx cmd sig m
  => m ()
applyOpts = do
  o <- sendIO $ execParser fullOpts
  when (o ^. #onlyLangInfo) do
    ln <- asks @(LangStatic sem ctx cmd) name
    sendIO do
      TIO.putStrLn $ ln <> " (" <> T.pack S.os <> ")"
      S.exitSuccess
  modify @(LangDynamic sem ctx) (& #wrapper %~ (o ^. #wrappers <>))
  when (o ^. #noWrapper) do
    modify @(LangDynamic sem ctx) (& #wrapper .~ Nothing)
  when (o ^. #nonInteractive) do
    modify @(LangDynamic sem ctx) (& #interactiveShell .~ False)
  modify @(LangDynamic sem ctx) (& #files .~ ((,False) <$> (o ^. #fileToLoad))) -- an abomination
  where
    fullOpts :: ParserInfo Opts
    fullOpts = info (defaultOpts <**> helper)
      (  fullDesc
      <> progDesc "exists s t. Lang s -> Lang t"
      <> header "The Programming Languages Zoo" )

{- Core -}

data RuntimeAction
  = RANop
  | RAPrint !Text
  deriving Generic

data RuntimeEnv (sem :: Type) (ctx :: Type) = MkRuntimeEnv
  { context    :: !ctx
  , replResult :: !(Maybe sem) }
  deriving Generic

mkRuntimeEnv :: ctx -> RuntimeEnv sem ctx
mkRuntimeEnv g = MkRuntimeEnv {context = g, replResult = Nothing}

type Evaluator (sem :: Type) (ctx :: Type) (cmd :: Type) =
  ctx -> cmd -> (Either LangError sem, ctx)

type RTS (sem :: Type) (ctx :: Type) (cmd :: Type) =
  RuntimeEnv sem ctx -> cmd -> (Either LangError (sem, RuntimeAction), RuntimeEnv sem ctx)

data LangStatic (sem :: Type) (ctx :: Type) (cmd :: Type) = MkLangStatic
  { name           :: !Text
  , options        :: ![(Text, Text, Text)] -- FIXME it's useless now
  , fileParser     :: !(Maybe (Text -> Either SyntaxError [cmd])) -- better safe than sorry
  , toplevelParser :: !(Maybe (Text -> Either SyntaxError cmd))
  , rts            :: !(RTS sem ctx cmd) }
  deriving Generic

data LangDynamic (sem :: Type) (ctx :: Type) = MkLangDynamic
  { environment      :: !(RuntimeEnv sem ctx)
  , interactiveShell :: !Bool
  , wrapper          :: !(Maybe [Text])
  , files            :: ![(Text, Bool)] }
  deriving Generic

defaultWrapper :: Maybe [Text]
defaultWrapper = Just ["rlwrap", "ledit"]

type Language sem ctx cmd sig m =
  ( Has (Reader (LangStatic sem ctx cmd)) sig m
  , Has (State (LangDynamic sem ctx)) sig m
  , Has (Lift IO) sig m )

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

{- Meta RTS -}

-- feels wonky
liftToRTS
  :: forall sem ctx cmd
  .  Eq sem
  => Evaluator sem ctx cmd
  -> (sem -> RuntimeAction)
  -> RTS sem ctx cmd
liftToRTS ev acts env c =
  let
    (r, newCtx) = ev (env ^. #context) c
    newEnv = env & #context .~ newCtx
   in case r of
     Left  e -> (Left e, newEnv)
     Right x -> (Right (x, acts x), newEnv)

executeRuntimeAction
  :: Language sem ctx cmd sig m
  => RuntimeAction
  -> m ()
executeRuntimeAction RANop       = pure ()
executeRuntimeAction (RAPrint t) = sendIO $ TIO.putStr t

runCommand
  :: forall sem ctx cmd sig m
  .  Language sem ctx cmd sig m
  => cmd
  -> m (Maybe sem)
runCommand c = do
  oldEnv <- gets @(LangDynamic sem ctx) environment
  ex <- asks @(LangStatic sem ctx cmd) rts
  let (res, newEnv) = ex oldEnv c
  modify @(LangDynamic sem ctx) (& #environment .~ newEnv)
  case res of
    Left  le      -> do
      printError le -- FIXME handle it higher
      pure Nothing
    Right (e, ra) -> do
      executeRuntimeAction @sem @ctx @cmd ra
      pure $ Just e

addFile
  :: forall sem ctx sig m
  .  Has (State (LangDynamic sem ctx)) sig m
  => Bool
  -> Text
  -> m ()
addFile interactive filename =
  modify @(LangDynamic sem ctx) (& #files %~ ( (filename, interactive) : ))

-- TODO use anonymous arguments?
-- anonymous
--   :: forall env sig m
--   .  Has (State (LangDynamic env)) sig m
--   => Text
--   -> m ()
-- anonymous t = do
--   addFile @env True t
--   modify @(LangDynamic env) (& #interactiveShell .~ False)

readFile
  :: forall sem ctx cmd sig m
  .  Language sem ctx cmd sig m
  => (Text -> Either SyntaxError [cmd])
  -> Text
  -> m [cmd]
readFile p filename = do
  fc <- handle dieOnIOError $ sendIO $ TIO.readFile $ T.unpack filename
  case p fc of
    Left  se -> printError se >> pure [] -- FIXME handle it higher
    Right cs -> pure cs
  where
    dieOnIOError :: IOError -> m Text
    dieOnIOError = throwIO . MkInternalError . T.pack . show

readToplevel
  :: forall sem ctx cmd sig m
  .  Language sem ctx cmd sig m
  => (Text -> Either SyntaxError cmd)
  -> m (Maybe cmd)
readToplevel p = do
  ln <- asks @(LangStatic sem ctx cmd) name
  let
    prompt     = ln <> "> "
    promptMore = T.replicate (T.length ln) " " <> "> "
  sendIO $ TIO.putStr prompt
  inp <- sendIO $ getMultiline promptMore
  if inp /= "\n"
     then case p inp of
       Left  se -> printError se >> pure Nothing -- FIXME handle it higher
       Right c  -> pure $ Just c
     else pure Nothing
  where
    getMultiline :: Text -> IO Text
    getMultiline pm = do
      inp <- TIO.getLine
      if not (T.null inp) && T.last inp == '\\'
         then do
           TIO.putStr pm
           ((T.init inp <> "\n") <>) <$> getMultiline pm
         else pure $ inp <> "\n"

useFile
  :: forall sem ctx cmd sig m
  .  Language sem ctx cmd sig m
  => (Text, Bool)
  -> m ()
useFile (filename, _) = do
  flp <- asks @(LangStatic sem ctx cmd) fileParser >>=
    maybeThrowIO (MkInternalError "This language can't load files.")
  cs  <- readFile @sem @ctx @cmd flp filename
  forM_ cs $ runCommand @sem @ctx

interactivePrinter
  :: forall sem ctx cmd sig m
  .  ( Language sem ctx cmd sig m
     , Show sem )
  => m ()
interactivePrinter = do
  rr <- gets @(LangDynamic sem ctx) (^. #environment . #replResult)
  let rrt = maybe "" (T.pack . show) rr
  unless (T.null rrt) $ sendIO $ TIO.putStrLn rrt

toplevel
  :: forall sem ctx cmd sig m
  .  ( Language sem ctx cmd sig m
     , Show sem )
  => m ()
toplevel = do
  tlp <- asks @(LangStatic sem ctx cmd) toplevelParser >>=
    maybeThrowIO (MkInternalError "This language has no toplevel.")
  languageName <- asks @(LangStatic sem ctx cmd) name
  sendIO $ TIO.putStrLn
    $  languageName <> " -- programming languages zoo\n"
    <> "Type " <> eofMarker <> " to exit."
  forever $ flip catches
    [ Handler handleUserInterrupt
    , Handler gracefulEOF
    ] do
        mc <- readToplevel @sem @ctx tlp
        case mc of
          Nothing -> modify @(LangDynamic sem ctx) (& #environment . #replResult .~ Nothing)
          Just c  -> do
            r <- runCommand @sem @ctx c
            modify @(LangDynamic sem ctx) (& #environment . #replResult .~ r)
        interactivePrinter @sem @ctx @cmd
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
  :: forall sem ctx cmd sig m
  .  ( Language sem ctx cmd sig m
     , Show sem )
  => m ()
mainPlan = do
  sendIO do
    myTid <- S.myThreadId
    void $ S.installHandler S.keyboardSignal (S.Catch $ S.throwTo myTid UserInterrupt) Nothing
    S.hSetBuffering S.stdout S.NoBuffering
  applyOpts @sem @ctx @cmd -- TODO parametric parser
  interactive <- gets @(LangDynamic sem ctx) interactiveShell
  mwr <- gets @(LangDynamic sem ctx) wrapper
  when interactive do
    flip (maybe (pure ())) mwr \wrs -> do
      myPath  <- sendIO S.getExecutablePath
      newArgs <- (++ ["--no-wrapper"]) <$> sendIO S.getArgs
      forM_ wrs \w ->
        handle @IOError (const $ pure ()) $ void $ sendIO $
          S.executeFile (T.unpack w) True (myPath : newArgs) Nothing
  fs <- reverse <$> gets @(LangDynamic sem ctx) files
  forM_ fs $ useFile @sem @ctx @cmd
  when interactive $ toplevel @sem @ctx @cmd
