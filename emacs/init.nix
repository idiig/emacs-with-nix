{ pkgs, epkgs }:

let
  dir = ./nix;
  # 读取该目录下所有文件名
  nixFiles = builtins.attrNames (builtins.readDir dir);
  # 过滤非nix文件后向每个文件传递参数（pkgs, epkgs）
  modules = map (file: import (dir + ("/" + file)) { inherit pkgs epkgs; })
	     (builtins.filter (name: builtins.match ".*\\.nix" name != null) nixFiles);
  # builtins.foldl' (累加函数) (初始值) (要处理的列表)
  merged = builtins.foldl' (acc: module: {
    packages = acc.packages ++ module.packages;
    config = acc.config + "\n" + module.config;
  })
    { packages = []; config = ""; }
    modules;
in
  merged
