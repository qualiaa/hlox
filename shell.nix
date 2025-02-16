{ pkgs ? import <nixpkgs> {} }:

let
  haskellPackages = pkgs.haskellPackages.extend (
    pkgs.haskell.lib.compose.packageSourceOverrides {
      crafting-interpreters = ./.;
    }
  );
in
haskellPackages.shellFor {
  packages = p: [ p.crafting-interpreters ];
  withHoogle = true;
  doBenchmark = true;
  buildInputs = (with haskellPackages; [ haskell-language-server cabal-install ]);
}
