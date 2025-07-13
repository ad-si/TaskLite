{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
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
            fourmolu
            gnumake
            haskell-language-server
            hlint
            mdbook
            mdbook-alerts
            mdbook-toc
            nodejs_24
            stack
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
