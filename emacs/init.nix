{ pkgs }:

let
  dir = ./emacs/nix;
  nixFiles = builtins.attrNames (builtins.readDir dir);
  modules = map (file: import (dir + ("/" + file)) { inherit pkgs; })
	       (builtins.filter (name: builtins.match ".*\\.nix" name != null) nixFiles);
in
builtins.foldl' (acc: module: acc // module) {} modules
