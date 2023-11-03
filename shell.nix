{ nixpkgs ? import <nixpkgs> {}, compiler ? "default", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, generics-sop, hpack, lib, web3 }:
      mkDerivation {
        pname = "pixura-contracts";
        version = "0.3.0.0";
        src = ./.;
        libraryHaskellDepends = [ base generics-sop ];
        libraryToolDepends = [ hpack ];
        prePatch = "hpack";
        homepage = "https://github.com/Pixura/pixura-contracts#readme";
        license = lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv
