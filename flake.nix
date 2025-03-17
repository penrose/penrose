{
  description = "Create beautiful diagrams just by typing notation in plain text";
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };
  outputs =
    {
      self,
      nixpkgs,
      flake-utils,
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs { inherit system; };
      in
      {
        devShell =
          with pkgs;
          mkShell {
            buildInputs = [
              cairo
              giflib
              libpng
              librsvg
              pango
              pixman
              pkg-config
              python310
              (yarn.override { nodejs = nodejs_18; })
            ];
          };
      }
    );
}
