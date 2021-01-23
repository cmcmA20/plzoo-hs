-- | This file contains all the common code used by the languages implemented in the PL Zoo.
module Zoo.Main
  ( zooMain
  ) where

import           Control.Algebra
import           Control.Carrier.Error.Church
import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import qualified Control.Concurrent           as S
import           Control.Effect.Exception
import           Control.Effect.Lift
import           Control.Effect.Runtime
import           Control.Lens
import           Control.Monad                (forM_, forever, unless, void,
                                               when)
import           Data.Generics.Labels         ()
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as TIO
import qualified GHC.IO.Exception             as S
import           Options.Applicative
import           Prelude                      hiding (readFile)
import qualified System.Exit                  as S
import qualified System.IO                    as S
import qualified System.Info                  as S
import qualified System.Posix.Signals         as S

import           Zoo.Core
import           Zoo.Debug
import           Zoo.Error
import           Zoo.Options

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
     , Has (Reader Debug) sig m
     , Has (State ctx) sig m
     , Show cmd
     , Show sem
     , Show ctx )
  => m ()
toplevel = do
  languageName <- asks @LangName unLangName
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
          runReader curCtx do
            printResult @sem @ctx r
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
     , Has (State ctx) sig m
     , Show sem )
  => Text
  -> m ()
useFile filename =
  defaultErrorHandler @SyntaxError do
    cs  <- readFile @cmd filename
    defaultErrorHandler @LangError $
      forM_ cs \c -> do
        r <- runCommand @cmd @sem @ctx c
        curCtx <- get @ctx
        runReader curCtx do
          printResult @sem @ctx r

zooMain
  :: forall clo cmd sem ctx sig m
  .  ( MetaRTS clo cmd sem ctx sig m
     , Show cmd
     , Show sem
     , Show ctx )
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

  when (defOpts ^. #nonInteractive . to not) do
    pure () -- TODO revert default line wrapper handler

  initState <- asks @(LangInit clo ctx) unLangInit
  runReader (defOpts ^. #metaDebug) $ evalState (initState opts) do
    forM_ (defOpts ^. #filesToLoad) $ useFile @clo @cmd @sem @ctx
    unless (defOpts ^. #nonInteractive) $
     toplevel @clo @cmd @sem @ctx
