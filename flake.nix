{
  description = "Parsing stackage";
  inputs.flake-compat = {
    url = "github:edolstra/flake-compat";
    flake = false;
  };
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nix-hs-utils = {
    url = "github:tbidne/nix-hs-utils";
    inputs.flake-compat.follows = "flake-compat";
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    inputs@{ flake-compat
    , flake-parts
    , nix-hs-utils
    , nixpkgs
    , self
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem = { pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          ghc-version = "ghc945";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
              apply-refact = prev.apply-refact_0_12_0_0;
              package-version = hlib.doJailbreak prev.package-version;
            };
          };
          mkPkg = returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "stackage-parse";
              root = ./.;
            };
          hs-dirs = "app src test";
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format {
              inherit compiler hs-dirs pkgs;
            };
            lint = nix-hs-utils.lint {
              inherit compiler hs-dirs pkgs;
            };
            lint-refactor = nix-hs-utils.lint-refactor {
              inherit compiler hs-dirs pkgs;
            };
          };
        };
      systems = [
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
