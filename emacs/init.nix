{ pkgs, epkgs }:

let
  dir = ./nix;
  nixFiles = builtins.attrNames (builtins.readDir dir);
  modules = map (file: import (dir + ("/" + file)) { inherit pkgs epkgs; })
	     (builtins.filter (name: builtins.match ".*\\.nix" name != null) nixFiles);
  merged = builtins.foldl' (acc: module: {
    packages = acc.packages ++ module.packages;
    config = acc.config + "\n" + module.config;
  }) { packages = []; config = ""; } modules;
in
  merged
