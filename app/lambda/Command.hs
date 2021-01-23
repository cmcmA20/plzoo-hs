module Command where

import           Control.Carrier.Reader
import           Control.Carrier.State.Strict
import           Control.Effect.Runtime
import           Control.Effect.Throw
import           Control.Lens
import           Data.Text                    (Text)
import           GHC.Generics                 (Generic)

import qualified Context                      as Con
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
  deriving Show

data ExCtx = MkExCtx
  { energy :: !Energy
  , depth  :: !Depth
  , ctx    :: !Con.Ctx }
  deriving (Generic, Show)

type Sem = Either Text Term

type Lambda sig m =
  ( Has (State ExCtx) sig m
  , Has (Throw LangError) sig m
  , Has Runtime sig m )

evalCmd :: Lambda sig m => Cmd -> m Sem
evalCmd (CExpr t) = do
  env <- get @ExCtx
  r <- runReader (env ^. #energy) .
    runReader (env ^. #depth) .
    runReader (env ^. #ctx) . normalize $ t
  pure $ Right r
evalCmd CContext = do
  env <- get @ExCtx
  r <- runReader (env ^. #ctx) Con.showAll
  pure $ Left r
evalCmd (CEnergy e) = do
  modify @ExCtx (& #energy .~ e)
  pure $ Left $ "I will evaluate " <> case e of
    EEager -> "eagerly"
    ELazy  -> "lazily"
evalCmd (CDepth d) = do
  modify @ExCtx (& #depth .~ d)
  pure $ Left $ "I will evaluate " <> case d of
    DDeep    -> "deeply"
    DShallow -> "shallowly"
evalCmd (CConst name) = do
  env <- get @ExCtx
  mv <- runReader (env ^. #ctx) (Con.lookupSafe name)
  case mv of
    Nothing -> do
      env' <- execState (env ^. #ctx) $ Con.define name Con.DConst
      modify @ExCtx (& #ctx .~ env')
      pure $ Left $ name <> " is a constant"
    Just _  -> throwError (LERuntime $ locate Nothing $ name <> " already exists")
evalCmd (CDefine name value) = do
  env <- get @ExCtx
  mv <- runReader (env ^. #ctx) (Con.lookupSafe name)
  case mv of
    Nothing -> do
      env' <- execState (env ^. #ctx) $ Con.define name $ Con.DTerm value
      modify @ExCtx (& #ctx .~ env')
      pure $ Left $ name <> " is defined"
    Just _  -> throwError (LERuntime $ locate Nothing $ name <> " already exists")
evalCmd CHelp = pure $ Left helpMessage
evalCmd CQuit = do
  rExit
  pure $ Left "bye"

helpMessage :: Text
helpMessage = "Toplevel directives:\n\
  \<expr>                       evaluate <expr>\n\
  \:lazy                        evaluate lazily (do not evaluate arguments)\n\
  \:eager                       evaluate eagrly (evaluate arguments immediately)\n\
  \:deep                        evaluate inside λ-abstraction\n\
  \:shallow                     do not evaluate inside λ-abstraction\n\
  \:constant x                  declare constant x\n\
  \:context                     print current definitions\n\
  \:help                        print this help\n\
  \:quit                        exit\n\
  \ \n\
  \Syntax:\n\
  \λ e                          λ-abstraction\n\
  \e1 e2                        application"
