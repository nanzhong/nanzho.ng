{
  description = "Dev env for nanzho.ng";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { nixpkgs, ... }@inputs: let
    forEachSystem = fn:
      nixpkgs.lib.genAttrs [
        "x86_64-linux" "aarch64-linux"
        "x86_64-darwin" "aarch64-darwin"
      ] (system: fn (import nixpkgs {
        inherit system;
      }));
  in {
    devShells = forEachSystem (pkgs: {
      default = pkgs.mkShell {
        buildInputs = with pkgs; [
          caddy
          emacs
          fish
          git
          nginx
          nodejs
          nodePackages.sass
        ];
      };
    });
  };
}
