{
  description = "idiig's Emacs Configuration with Nix Flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
	pkgs = nixpkgs.legacyPackages.${system};
	emacsConfig = import ./emacs/init.nix { inherit pkgs; };
      in {
	packages = {
	  emacs = emacsConfig;
	};
      }
    );
}
