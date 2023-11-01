{ mkDerivation, base, lib, really-safe-money, validity }:
mkDerivation {
  pname = "actus";
  version = "0.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base really-safe-money validity ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base ];
  license = "unknown";
  mainProgram = "actus";
}
