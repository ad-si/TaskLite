{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
            gawk  # Needed by packages `network` and `old-time`
            git-cliff
            gnumake
            haskell.compiler.ghc910
            haskellPackages.cabal-fmt
            haskellPackages.cabal-install
            haskellPackages.fourmolu
            (pkgs.haskell-language-server.override {
              supportedGhcVersions = [ "9103" ];
            })
            haskellPackages.hlint
            haskellPackages.stack
            mdbook
            mdbook-toc
            nodejs_22  # For building the webapp
            sqlite
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
