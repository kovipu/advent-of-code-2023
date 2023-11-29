{
  description = "AoC fleikki";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  nixConfig.bash-prompt-prefix = "🎅 ";

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        haskellPackages = pkgs.haskell.packages.ghc945;
      in
      {
        devShell = pkgs.mkShell {
          shellHook = ''
            alias day="cabal run aoc-template-exe -- --day"
          '';
          buildInputs = with pkgs; [
            haskellPackages.haskell-language-server
            haskellPackages.cabal-install
          ];
        };
      });
}
