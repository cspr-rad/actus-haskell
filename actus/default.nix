{ mkDerivation, base, binary-search, lib, pretty-show
, really-safe-money, time, validity
}:
mkDerivation {
  pname = "actus";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base binary-search pretty-show really-safe-money time validity
  ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "actus";
}
