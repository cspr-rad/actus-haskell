{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Actus.Term.TestUtils where

import Actus.Term.Class
import Actus.TestUtils
import Data.Aeson as JSON
import Path
import Path.IO
import Test.Syd
import Test.Syd.Validity

resolveTermTestDataDir :: (MonadIO m) => m (Path Abs Dir)
resolveTermTestDataDir = liftIO $ do
  testDataDir <- resolveTestDataDir
  resolveDir testDataDir "terms"

termSpec ::
  forall term.
  ( Show term,
    Eq term,
    GenValid term,
    IsTerm term,
    FromJSON term,
    ToJSON term
  ) =>
  Path Abs Dir ->
  String ->
  Spec
termSpec testDataDir name = do
  it "It roundtrips through Term" $
    forAllValid $ \term -> do
      let rendered = toTerm (term :: term)
      context (show rendered) $ case fromTerm rendered of
        Left err -> expectationFailure err
        Right term' -> term' `shouldBe` term

  parseTestDataSpec @term testDataDir name
