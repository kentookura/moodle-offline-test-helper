{
  inputs = {
    nixpkgs.url = "nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        libraries = with pkgs.elmPackages;[
          elm
          elm-test
          elm-live
          elm-format
          elm-language-server
        ];

        packages = with pkgs; [
        ];
      in
      {
        devShell = pkgs.mkShell {
          buildInputs = with pkgs;  libraries ++ [
          ];
        };
      });
}
