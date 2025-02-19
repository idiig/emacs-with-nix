{ pkgs }:

{
  packages = with pkgs; [
    git
  ] ++ with pkgs.emacsPackages; [
    magit
  ];

  config = {
    enable = true;
    extraConfig = builtins.readFile ../config/git.el;
  };
}
