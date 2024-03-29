{
  description = "actus-haskell";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs?ref=nixos-23.11";
    pre-commit-hooks.url = "github:cachix/pre-commit-hooks.nix";
    validity.url = "github:NorfairKing/validity";
    validity.flake = false;
    autodocodec.url = "github:NorfairKing/autodocodec";
    autodocodec.flake = false;
    safe-coloured-text.url = "github:NorfairKing/safe-coloured-text";
    safe-coloured-text.flake = false;
    fast-myers-diff.url = "github:NorfairKing/fast-myers-diff";
    fast-myers-diff.flake = false;
    sydtest.url = "github:NorfairKing/sydtest";
    sydtest.flake = false;
    really-safe-money.url = "github:NorfairKing/really-safe-money";
    really-safe-money.flake = false;
    dekking.url = "github:NorfairKing/dekking";
    dekking.flake = false;
    actus-spec.url = "github:cspr-rad/actus-spec";
  };

  outputs =
    { self
    , nixpkgs
    , pre-commit-hooks
    , validity
    , safe-coloured-text
    , autodocodec
    , fast-myers-diff
    , sydtest
    , really-safe-money
    , dekking
    , actus-spec
    }:
    let
      system = "x86_64-linux";
      nixpkgsFor = nixpkgs: import nixpkgs {
        inherit system;
        config.allowUnfree = true;
      };
      pkgs = nixpkgsFor nixpkgs;
      allOverrides = pkgs.lib.composeManyExtensions [
        (pkgs.callPackage (validity + "/nix/overrides.nix") { })
        (pkgs.callPackage (autodocodec + "/nix/overrides.nix") { })
        (pkgs.callPackage (safe-coloured-text + "/nix/overrides.nix") { })
        (pkgs.callPackage (fast-myers-diff + "/nix/overrides.nix") { })
        (pkgs.callPackage (sydtest + "/nix/overrides.nix") { })
        (pkgs.callPackage (really-safe-money + "/nix/overrides.nix") { })
        (pkgs.callPackage (dekking + "/nix/overrides.nix") { })
        self.overrides.${system}
      ];
      haskellPackagesFor = nixpkgs: (nixpkgsFor nixpkgs).haskellPackages.extend allOverrides;
      haskellPackages = haskellPackagesFor nixpkgs;
      actusSpec = actus-spec.packages.${system}.spec;
    in
    {
      overrides.${system} = pkgs.callPackage ./nix/overrides.nix {
        inherit actusSpec;
      };
      overlays.${system} = import ./nix/overlay.nix;
      packages.${system}.default = haskellPackages.actus;
      checks.${system} = {
        release = haskellPackages.actusRelease;
        selftest = pkgs.runCommand "actus-haskell-selftest" { } ''
          ${haskellPackages.actusRelease}/bin/actus-tester ${haskellPackages.actusRelease}/bin/actus-test-harness > $out 2>&1
        '';
        pre-commit = pre-commit-hooks.lib.${system}.run {
          src = ./.;
          hooks = {
            hlint.enable = true;
            hpack.enable = true;
            ormolu.enable = true;
            nixpkgs-fmt.enable = true;
            nixpkgs-fmt.excludes = [ ".*/default.nix" ];
            cabal2nix.enable = true;
            tagref.enable = true;
          };
        };
      };
      devShells.${system}.default = haskellPackages.shellFor {
        name = "actus-haskell-shell";
        packages = p: [ p.actus ];
        withHoogle = true;
        doBenchmark = true;
        buildInputs = (with pkgs; [
          cabal-install
          zlib
        ]) ++ (with pre-commit-hooks.packages.${system};
          [
            hlint
            hpack
            nixpkgs-fmt
            ormolu
            cabal2nix
            tagref
          ]);
        shellHook = self.checks.${system}.pre-commit.shellHook;
        ACTUS_SPEC = actusSpec;
      };
    };
}
