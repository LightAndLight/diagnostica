{
  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let 
        pkgs = import nixpkgs { inherit system; };
      in {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs; [
            haskellPackages.ghc
            cabal-install
            haskell-language-server

            just
            haskellPackages.fourmolu
            fd
          ];
        };
      }
    );
}
