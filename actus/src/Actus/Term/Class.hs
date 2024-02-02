{-# LANGUAGE LambdaCase #-}

module Actus.Term.Class where

import Actus.Types
import Data.Text (Text)

class IsTerm term where
  toTerm :: term -> Term
  fromTerm :: Term -> Either String term

instance IsTerm Text where
  toTerm = TermText
  fromTerm = \case
    TermText t -> Right t
    t -> Left $ "Invalid term type for term of type Text: " <> show t
