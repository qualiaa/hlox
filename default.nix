{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.developPackage {
  name = "crafting-interpreters";
  root = ./.;
}
