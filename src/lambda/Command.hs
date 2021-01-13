module Command where

import Control.Carrier.Reader
import Control.Carrier.State.Strict
import Control.Effect.Throw
import Control.Lens
import Data.Text (Text)
import GHC.Generics (Generic)

import qualified Context as Con
import           Normalize
import           Syntax
import           Zoo

data Cmd
  = CDefine !Text !Term
  | CConst !Text
  | CExpr !Term
  | CHelp
  | CQuit
  | CContext
  | CEnergy !Energy
  | CDepth !Depth

data ExCtx = MkExCtx
  { energy :: !Energy
  , depth  :: !Depth
  , ctx    :: !Con.Ctx }
  deriving Generic

type Sem = Either Text Term

type Lambda sig m =
  ( Has (State ExCtx) sig m
  , Has (Throw LangError) sig m
  , Has Runtime sig m )

evalCmd :: Lambda sig m => Cmd -> m Sem
evalCmd (CExpr t) = do
  env <- get @ExCtx
  r <- runReader (env ^. #energy) . runReader (env ^. #depth) $ normalize t
  pure $ Right r
evalCmd CContext = do
  env <- get @ExCtx
  r <- runReader (env ^. #ctx) Con.showAll
  pure $ Left r
evalCmd (CEnergy e) = do
  modify @ExCtx (& #energy .~ e)
  pure $ Left "I will evaluate FIXME_ENERGY"
evalCmd (CDepth d) = do
  modify @ExCtx (& #depth .~ d)
  pure $ Left "I will evaluate FIXME_DEPTH"
evalCmd (CConst name) = do
  env <- get @ExCtx
  mv <- runReader (env ^. #ctx) (Con.lookup name)
  case mv of
    Nothing -> do
      env' <- execState (env ^. #ctx) $ Con.define name Nothing
      modify @ExCtx (& #ctx .~ env')
      pure $ Left $ name <> " is a constant"
    Just _  -> throwError (LERuntime $ locate Nothing $ name <> " already exists")
evalCmd (CDefine name value) = do
  env <- get @ExCtx
  mv <- runReader (env ^. #ctx) (Con.lookup name)
  case mv of
    Nothing -> do
      env' <- execState (env ^. #ctx) $ Con.define name (Just value)
      modify @ExCtx (& #ctx .~ env')
      pure $ Left $ name <> " is defined"
    Just _  -> throwError (LERuntime $ locate Nothing $ name <> " already exists")
evalCmd CHelp = pure $ Left "god help you"
evalCmd CQuit = do
  rExit
  pure $ Left "bye"
