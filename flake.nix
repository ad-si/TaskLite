{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-25.05";
    utils.url = "github:numtide/flake-utils";
  };

  outputs =
    {
      self,
      nixpkgs,
      utils,
    }:
    utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            bash
            cargo  # To install term-transcript
            coreutils
            fd
            flyctl
            gnumake
            haskell.compiler.ghc98
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            haskellPackages.haskell-language-server
            haskellPackages.hlint
            haskellPackages.stack
            mdbook
            mdbook-alerts
            mdbook-toc
            nodejs_22  # For building the webapp
            zlib
          ];
          shellHook = ''
            cargo install --locked term-transcript-cli@0.4.0
          '';
        };
        formatter = pkgs.nixfmt-tree; # Format this file with `nix fmt`
      }
    );
}
