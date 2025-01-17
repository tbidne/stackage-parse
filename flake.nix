{
  description = "Parsing stackage";

  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.nix-hs-utils.url = "github:tbidne/nix-hs-utils";
  inputs.nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  outputs =
    inputs@{
      flake-parts,
      nix-hs-utils,
      nixpkgs,
      self,
    }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      perSystem =
        { pkgs, ... }:
        let
          hlib = pkgs.haskell.lib;
          ghc-version = "ghc9101";
          compiler = pkgs.haskell.packages."${ghc-version}";
          mkPkg =
            returnShellEnv:
            nix-hs-utils.mkHaskellPkg {
              inherit compiler pkgs returnShellEnv;
              name = "stackage-parse";
              root = ./.;

              devTools = [
                (hlib.dontCheck compiler.cabal-fmt)
                (hlib.dontCheck compiler.haskell-language-server)
                pkgs.nixfmt-rfc-style
              ];
            };
          compilerPkgs = {
            inherit compiler pkgs;
          };
        in
        {
          packages.default = mkPkg false;
          devShells.default = mkPkg true;

          apps = {
            format = nix-hs-utils.format compilerPkgs;
            #lint = nix-hs-utils.lint compilerPkgs;
            #lint-refactor = nix-hs-utils.lint-refactor compilerPkgs;
          };
        };
      systems = [
        "aarch64-darwin"
        "aarch64-linux"
        "x86_64-darwin"
        "x86_64-linux"
      ];
    };
}
