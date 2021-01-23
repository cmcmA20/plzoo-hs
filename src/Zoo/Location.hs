-- | Location helpers
module Zoo.Location where

import           Data.Kind    (Type)
import           Data.Text    (Text)
import qualified Data.Text    as T
import           GHC.Generics (Generic)

data Location
  = LNowhere -- ^ No location
  | LLocation !Integer !Integer -- ^ Delimited location (line, column)
  deriving (Eq, Generic, Ord)

showLocation :: Location -> Text
showLocation LNowhere        = "unknown location"
showLocation (LLocation r c) = "line " <> T.pack (show r) <> ", column " <> T.pack (show c)

instance Show Location where
  show = T.unpack . showLocation

type Located :: Type -> Type
data Located a = MkLocated
  { content :: !a
  , loc     :: !Location }
  deriving (Generic, Show)

locate :: Maybe Location -> a -> Located a
locate Nothing  x = MkLocated x LNowhere
locate (Just l) x = MkLocated x l
