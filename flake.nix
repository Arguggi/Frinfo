{
  description = "frinfo";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, flake-compat }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        packageName = "frinfo";
        frinfo = pkgs.haskellPackages.callCabal2nix packageName self rec {
          slave-thread = pkgs.haskell.lib.dontCheck pkgs.haskellPackages.slave-thread;
        };
      in
      {
        packages.${packageName} = frinfo;
        defaultPackage = frinfo;
        devShell = pkgs.mkShell {
          buildInputs = [
            pkgs.zlib
            pkgs.libxml2
            pkgs.pkg-config
            pkgs.haskellPackages.cabal-install
            pkgs.haskellPackages.haskell-language-server
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
