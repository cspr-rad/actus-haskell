{ mkDerivation, aeson, autodocodec, base, containers, genvalidity
, genvalidity-containers, genvalidity-sydtest
, genvalidity-sydtest-aeson, genvalidity-text, genvalidity-time
, lib, path, path-io, pretty-show, QuickCheck, really-safe-money
, really-safe-money-autodocodec, really-safe-money-gen, sydtest
, sydtest-discover, text, time, validity, validity-containers
, validity-text, validity-time
}:
mkDerivation {
  pname = "actus";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base containers pretty-show really-safe-money
    really-safe-money-autodocodec text time validity
    validity-containers validity-text validity-time
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base genvalidity genvalidity-containers genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text genvalidity-time path
    path-io QuickCheck really-safe-money really-safe-money-gen sydtest
    text time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "actus";
}
