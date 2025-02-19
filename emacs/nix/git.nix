{ pkgs, epkgs }:

{
  packages = with pkgs; [
    git
  ] ++ ([
    epkgs.magit
  ]);

  config = builtins.readFile ../config/git.el;
}
