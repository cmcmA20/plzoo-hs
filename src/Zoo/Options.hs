-- | Command line options parsing
module Zoo.Options where

import Control.Algebra
import Control.Carrier.Reader
import Control.Effect.Exception
import Control.Effect.Lift
import Data.Text (Text)
import GHC.Generics (Generic)
import Options.Applicative

import Zoo.Core
import Zoo.Debug

data DefaultOpts = MkDefaultOpts
  { nonInteractive :: !Bool
  , onlyLangInfo   :: !Bool
  , metaDebug      :: !Debug
  , filesToLoad    :: ![Text] }
  deriving (Generic, Show)

defaultOpts :: Parser DefaultOpts
defaultOpts = MkDefaultOpts
  <$> switch (short 'n'
    <> help "Do not run the interactive toplevel")
  <*> switch (short 'v'
    <> help "Print language information and exit")
  <*> option auto (long "meta-debug"
    <> showDefault
    <> value defaultDebug
    <> help "Print debugging info")
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
    fullOptInfo p = info (((,) <$> p <*> defaultOpts) <**> helper)
      (  fullDesc
      <> progDesc "Too many levels of abstraction"
      <> header "The Programming Languages Zoo" )
