{ mkDerivation, base, generics-sop, lib, zlib, web3-ethereum }:
mkDerivation {
  pname = "pixura-contracts";
  version = "0.3.0.0";
  src = ./.;
  libraryHaskellDepends = [ base generics-sop web3-ethereum ];
  libraryPkgconfigDepends = [ zlib ];
  homepage = "https://github.com/Pixura/pixura-contracts#readme";
  license = lib.licenses.bsd3;
}
