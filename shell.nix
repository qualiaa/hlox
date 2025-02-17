{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskellPackages.extend (
    pkgs.haskell.lib.compose.packageSourceOverrides {
      hlox = ./.;
    }
  );
in
haskellPackages.shellFor {
  packages = p: [ p.hlox ];
  withHoogle = true;
  doBenchmark = true;
  buildInputs = (with haskellPackages; [ haskell-language-server cabal-install ]);
}
