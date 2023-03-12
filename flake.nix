# SPDX-FileCopyrightText: 2021 Serokell <https://serokell.io/>
#
# SPDX-License-Identifier: CC0-1.0

{
  description = "JIRA WSJF Calculator";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        haskellPackages = pkgs.haskell.packages.ghc902;

        jailbreakUnbreak = pkg:
          pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        packageName = "JobWsjf";
      in {
        packages.${packageName} =
          haskellPackages.callCabal2nix packageName self rec {
            # Dependency overrides go here
          };

        doHaddock = false;
        defaultPackage = self.packages.${system}.${packageName};

        devShell = haskellPackages.shellFor {
          buildInputs = with haskellPackages; [
            cabal-install
            ghcid
            hlint
            pkgs.pcre
            pkgs.zlib
          ];
          packages = haskellPackages: [
            self.packages.${system}.${packageName}
          ];
          withHoogle = true;
        };

      });
}
