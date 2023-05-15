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
    inputs.nixpkgs.follows = "nixpkgs";
  };
  inputs.nixpkgs.url = "github:nixos/nixpkgs?rev=5a1dc8acd977ff3dccd1328b7c4a6995429a656b";
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
          ghc-version = "ghc927";
          compiler = pkgs.haskell.packages."${ghc-version}".override {
            overrides = final: prev: {
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
