module Zoo.Parser where

import Control.Applicative
import Control.Carrier.State.Strict
import Control.Carrier.Throw.Either
import Control.Lens
import Data.Kind (Type)
import GHC.Generics (Generic)

import Zoo.Core
import Zoo.Error
import Zoo.Location

data ParserState (i :: Type) (u :: Type) = MkParserState
  { inputStream :: [i]
  , userState   :: u }
  deriving Generic

newtype Parser (i :: Type) (u :: Type) (o :: Type) = MkParser { unParser
  :: forall (sig :: SigKind) (m :: CarrierKind)
  .  ( Has (Throw SyntaxError) sig m
     , Has (State (ParserState i u)) sig m )
  => m o }
  deriving Functor

instance Applicative (Parser i u) where
  pure :: a -> Parser i u a
  pure x = MkParser $ pure x

  (<*>) :: Parser i u (a -> b) -> Parser i u a -> Parser i u b
  MkParser fp <*> MkParser fx = MkParser do
    f <- fp
    f <$> fx

instance Alternative (Parser i u) where
  empty :: Parser i u a
  empty = MkParser $ throwError $ SEParse LNowhere -- FIXME location

  (<|>) :: Parser i u a -> Parser i u a -> Parser i u a
  MkParser fx <|> MkParser fy = MkParser do
    ps <- get @(ParserState i u)
    w <- runThrow @SyntaxError fx
    case w of
      Left  _   -> put ps >> fy
      Right res -> pure res

instance Monad (Parser i u) where
  (>>=) :: Parser i u a -> (a -> Parser i u b) -> Parser i u b
  MkParser fx >>= g = MkParser do
    x <- fx
    unParser $ g x

runParser
  :: forall i u o sig m
  .  Has (Throw SyntaxError) sig m
  => u -- ^ user state
  -> [i] -- ^ input stream
  -> Parser i u o -- ^ parser
  -> m o
runParser st inp = evalState (MkParserState inp st) . unParser

pSatisfy :: forall i u. (i -> Bool) -> Parser i u i
pSatisfy predi = MkParser do
  ps <- get @(ParserState i u)
  case ps ^. #inputStream of
    []     -> throwError $ SEParse LNowhere -- FIXME location
    c:inp' -> modify @(ParserState i u) (& #inputStream .~ inp') >> if predi c
      then pure c
      else throwError $ SEParse LNowhere -- FIXME location

pEOF :: forall i u. Parser i u ()
pEOF = MkParser do
  ps <- get @(ParserState i u)
  case ps ^. #inputStream of
    [] -> pure ()
    _  -> throwError $ SEParse LNowhere -- FIXME location

pSingle :: forall i u. Eq i => i -> Parser i u i
pSingle = pSatisfy . (==)

pAny :: forall i u. Parser i u i
pAny = MkParser do
  ps <- get @(ParserState i u)
  case ps ^. #inputStream of
    []     -> throwError $ SEParse LNowhere -- FIXME location
    c:inp' -> modify @(ParserState i u) (& #inputStream .~ inp') >> pure c

pFail :: Parser i u a
pFail = MkParser $ throwError $ SEParse LNowhere -- FIXME location
