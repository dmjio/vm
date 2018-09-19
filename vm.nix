{ mkDerivation, base, microlens-platform, mtl, stdenv }:
mkDerivation {
  pname = "virtual-machine";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = false;
  isExecutable = true;
  executableHaskellDepends = [ base microlens-platform mtl ];
  homepage = "https://github.com/dmjio/vm";
  description = "A tiny virtual machine";
  license = stdenv.lib.licenses.bsd3;
}
