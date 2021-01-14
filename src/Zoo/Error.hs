-- | Error handling
module Zoo.Error where

import           Control.Algebra
import           Control.Carrier.Error.Church
import           Control.Effect.Exception
import           Control.Effect.Lift
import           Data.Generics.Labels ()
import           Data.Text (Text)
import qualified Data.Text                       as T
import qualified Data.Text.IO                    as TIO

import Zoo.Location

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
  deriving (Eq, Ord)

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
