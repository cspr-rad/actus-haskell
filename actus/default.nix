{ mkDerivation, aeson, autodocodec, base, bytestring, containers
, genvalidity, genvalidity-containers, genvalidity-scientific
, genvalidity-sydtest, genvalidity-sydtest-aeson, genvalidity-text
, genvalidity-time, lib, path, path-io, pretty-show, QuickCheck
, really-safe-money, really-safe-money-autodocodec
, really-safe-money-gen, scientific, sydtest, sydtest-discover
, text, time, validity, validity-containers, validity-scientific
, validity-text, validity-time
}:
mkDerivation {
  pname = "actus";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson autodocodec base bytestring containers pretty-show
    really-safe-money really-safe-money-autodocodec scientific text
    time validity validity-containers validity-scientific validity-text
    validity-time
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [
    aeson base genvalidity genvalidity-containers
    genvalidity-scientific genvalidity-sydtest
    genvalidity-sydtest-aeson genvalidity-text genvalidity-time path
    path-io QuickCheck really-safe-money really-safe-money-gen sydtest
    text time
  ];
  testToolDepends = [ sydtest-discover ];
  license = "unknown";
}
