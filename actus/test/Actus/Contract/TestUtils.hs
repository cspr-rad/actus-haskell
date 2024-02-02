{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Actus.Contract.TestUtils where

import Actus.Contract.Class
import Test.Syd
import Test.Syd.Validity

contractSpec ::
  forall contract.
  (Show contract, Eq contract, GenValid contract, IsContract contract) =>
  Spec
contractSpec =
  it "It roundtrips through Contract" $
    forAllValid $ \contract -> do
      let rendered = toContract (contract :: contract)
      context (show rendered) $ case fromContract rendered of
        Left err -> expectationFailure err
        Right contract' -> contract' `shouldBe` contract
