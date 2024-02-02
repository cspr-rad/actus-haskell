module Actus.Contract.Class where

import Actus.Types

class IsContract contract where
  toContract :: contract -> Contract
  fromContract :: Contract -> Either String contract
