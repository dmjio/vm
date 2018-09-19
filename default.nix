{}:
let
  nixpkgs = import <nixpkgs> {};
in
  nixpkgs.haskellPackages.callPackage ./vm.nix {}
