{
  # inspired by: https://serokell.io/blog/practical-nix-flakes#packaging-existing-applications
  description = "A Hello World in Haskell with a dependency and a devShell";
  inputs =
    {
      nixpkgs.url = "nixpkgs";
      hs-web3 =
        { url = "hs-web3";
          inputs.nixpkgs.url = "nixpkgs";
        };
    };
  outputs = { self, nixpkgs, hs-web3 }:
    let
      supportedSystems = [ "x86_64-linux" "x86_64-darwin" ];
      forAllSystems = f: nixpkgs.lib.genAttrs supportedSystems (system: f system);
      nixpkgsFor = forAllSystems (system: import nixpkgs {
        inherit system;
        overlays = [ (self.overlay system) ];
      });
    in
    {
      overlay = (system: final: prev: {
        pixura-contracts = final.haskell.packages.ghc924.callPackage (import ./default.nix) {
          inherit (final) zlib;
          inherit (hs-web3.packages.${system}) web3-ethereum;
        };
      });
      packages = forAllSystems (system: {
         pixura-contracts = nixpkgsFor.${system}.pixura-contracts;
      });
      defaultPackage = forAllSystems (system: self.packages.${system}.pixura-contracts);
      checks = self.packages;
      devShell = forAllSystems (system:
        let haskellPackages = nixpkgsFor.${system}.haskell.packages.ghc924;
        in haskellPackages.shellFor
          {
           packages = p: [self.packages.${system}.pixura-contracts];
           withHoogle = true;
           buildInputs = with haskellPackages;
             [
              haskell-language-server
              cabal-install
              ];
           # Change the prompt to show that you are in a devShell
           # shellHook = "export PS1='\\e[1;34mdev > \\e[0m'";
           });
    };
}
