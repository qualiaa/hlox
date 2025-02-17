{ pkgs ? import <nixpkgs> {} }:

pkgs.haskellPackages.developPackage {
  name = "hlox";
  root = ./.;
}
