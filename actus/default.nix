{ mkDerivation, aeson, autodocodec, base, genvalidity
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-time, lib, path, path-io, pretty-show
, really-safe-money, really-safe-money-autodocodec
, really-safe-money-gen, sydtest, sydtest-discover, text, time
, validity, validity-text
}:
mkDerivation {
  pname = "actus";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base pretty-show really-safe-money
    really-safe-money-autodocodec text time validity validity-text
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base genvalidity genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text genvalidity-time path
    path-io really-safe-money really-safe-money-gen sydtest text time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
  mainProgram = "actus";
}
