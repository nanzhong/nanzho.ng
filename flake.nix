{
  description = "Dev env for nanzho.ng";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay = {
      url = "github:nix-community/emacs-overlay";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, ... }@inputs:
    with inputs;
    flake-utils.lib.eachDefaultSystem(system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            inputs.emacs-overlay.overlay
          ];
        };
        emacs = (pkgs.emacsGit-nox.override {
          nativeComp = true;
          withSQLite3 = true;
        });

        deps = with pkgs; [
          emacs
          fish
          git
          nginx
          nodejs
          nodePackages.sass
        ];
      in {
        devShell = with pkgs;
          mkShell {
            buildInputs = deps;
          };
      }
    );
}
