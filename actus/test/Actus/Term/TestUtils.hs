{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actus.Term.TestUtils where

import Actus.Term.Class
import Test.Syd
import Test.Syd.Validity

termSpec ::
  forall term.
  (Show term, Eq term, GenValid term, IsTerm term) =>
  Spec
termSpec =
  it "It roundtrips through Term" $
    forAllValid $ \term -> do
      let rendered = toTerm (term :: term)
      context (show rendered) $ case fromTerm rendered of
        Left err -> expectationFailure err
        Right term' -> term' `shouldBe` term
