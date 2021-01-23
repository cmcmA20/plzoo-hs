module Context where

import           Control.Effect.Reader
import           Control.Effect.State
import           Control.Effect.Throw
import           Data.HashMap.Strict   (HashMap)
import qualified Data.HashMap.Strict   as HM
import           Data.Text             (Text)

import           Printer
import           Syntax
import           Zoo

data Decl = DConst | DTerm Term
  deriving Show

newtype Ctx = MkCtx { unCtx :: HashMap Text Decl }
  deriving Show

empty :: Ctx
empty = MkCtx HM.empty

lookupSafe
  :: ( Has (Reader Ctx) sig m )
  => Text
  -> m (Maybe Decl)
lookupSafe name = asks @Ctx (HM.lookup name . unCtx)

lookup
  :: ( Has (Reader Ctx) sig m
     , Has (Throw LangError) sig m )
  => Text
  -> m Decl
lookup name = lookupSafe name >>=
  maybeThrow (LERuntime $ locate Nothing $ "unknown identifier " <> name)

define :: Has (State Ctx) sig m => Text -> Decl -> m ()
define name mTerm = modify @Ctx (MkCtx . HM.insert name mTerm . unCtx)

showAll :: Has (Reader Ctx) sig m => m Text
showAll = do
  c <- asks @Ctx unCtx
  pure $ HM.foldlWithKey' (\t k v -> t <> showOneEntry k v <> "\n") "" c
  where
  showOneEntry :: Text -> Decl -> Text
  showOneEntry name DConst    = "#constant " <> name <> " ;"
  showOneEntry name (DTerm t) = name <> " := " <> showTerm t <> " ;"
