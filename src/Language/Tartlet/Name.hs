module Language.Tartlet.Name where

import Data.Char (chr, ord)
import Data.Hashable
import Data.Text (Text)
import Data.Text qualified as T

newtype Name = MkName Text
  deriving (Eq, Ord, Hashable, Show)

showName :: Name -> Text
showName (MkName t) = t

toName :: Int -> Name
toName i
  | i < 0     = error "name generation"
  | otherwise = MkName $ T.singleton (chr (ord 'a' + i `mod` 26)) <> T.pack (show $ i `div` 26)
