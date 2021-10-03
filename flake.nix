{
  description = "kaleidoscope-hs";

  inputs.haskellNix.url = "github:input-output-hk/haskell.nix/7421a25fb842e188eff6612f31a766e479f973a5";
  inputs.nixpkgs.follows = "haskellNix/nixpkgs-unstable";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = { self, nixpkgs, flake-utils, haskellNix }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlay = self: _: {
          LLVM-12 = pkgs.llvm_12.dev;
          hsPkgs =
            self.haskell-nix.project' rec {
              src = ./.;
              compiler-nix-name = "ghc8105";
              shell = {
                tools = {
                  cabal = { };
                  ghcid = { };
                  haskell-language-server = { };
                  hlint = { };
                };
                buildInputs =
                  let
                    ormolu = pkgs.haskell-nix.tool compiler-nix-name "ormolu" "latest";
                    ormolu-wrapped = pkgs.writeShellScriptBin "ormolu" ''
                      ${ormolu}/bin/ormolu --ghc-opt=-XImportQualifiedPost $@
                    '';
                  in
                  [ ormolu-wrapped ];
              };
            };
        };
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            haskellNix.overlay
            overlay
          ];
        };
        flake = pkgs.hsPkgs.flake { };
      in
      flake // { defaultPackage = flake.packages."kaleidoscope-hs:exe:kaleidoscope-hs-exe"; }
    );
}
