{ inputs, pkgs, emacsPackages }: let
  inherit (builtins) readDir;
  inherit (pkgs) runCommand;
  inherit (pkgs.lib) attrNames attrsToList filter functionArgs hasAttr mergeAttrsList pipe readFile remove;
  packagesDir = ./.;
  packageSources = inputs // {
    # nano = inputs.nano-emacs;
  };
  importFile = dir: let
    packageFunction = import "${packagesDir}/${dir}";
  in emacsPackages.callPackage packageFunction (
    pipe ({
      elispFileVersion = file: let
        output = runCommand "${baseNameOf file}-version" { } ''
          ${emacsPackages.emacs}/bin/emacs -Q --batch \
            --eval "(require 'lisp-mnt)" \
            --eval '(setq pkg-version (lm-version "${file}"))' \
            --eval '(find-file (getenv "out"))' \
            --eval '(insert pkg-version)' \
            --eval '(save-buffer)'
        '';
      in readFile output;
      pkgFileVersion = file: let
        output = runCommand "${baseNameOf file}-version" { } ''
          ${emacsPackages.emacs}/bin/emacs -Q --batch \
            --eval '(find-file "${file}")' \
            --eval '(setq pkg-version (caddr (read (current-buffer))))' \
            --eval '(find-file (getenv "out"))' \
            --eval '(insert pkg-version)' \
            --eval '(save-buffer)'
        '';
      in readFile output;
      normalizeVersion = name: version: let
        output = runCommand "${name}-normalized-version" { } ''
          ${emacsPackages.emacs}/bin/emacs -Q --batch \
            --load package \
            --eval '(setq pkg-version (package-version-join (version-to-list "${version}")))' \
            --eval '(find-file (getenv "out"))' \
            --eval '(insert pkg-version)' \
            --eval '(save-buffer)'
        '';
      in readFile output;
      genericBuild = emacsPackages.callPackage "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/generic.nix" { };
      elpa2nix = "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/elpa2nix.el";
      melpa2nix = "${inputs.nixpkgs}/pkgs/applications/editors/emacs/build-support/melpa2nix.el";
    } // (if hasAttr dir packageSources then { package_src = packageSources.${dir}; } else { })
    ) [
      attrsToList
      # => [ 
      #      { name = "elispFileVersion"; value = <function>; }
      #      { name = "pkgFileVersion"; value = <function>; }
      #      { name = "normalizeVersion"; value = <function>; }
      #      { name = "genericBuild"; value = <function>; }
      #      { name = "elpa2nix"; value = "/path/to/elpa2nix.el"; }
      #      { name = "melpa2nix"; value = "/path/to/melpa2nix.el"; }
      #    ]
      (filter ({ name, ... }: hasAttr name (functionArgs packageFunction)))
      # => 假设 packageFunction 需要 elispFileVersion 和 genericBuild
      # => [ 
      #      { name = "elispFileVersion"; value = <function>; }
      #      { name = "genericBuild"; value = <function>; }
      #    ]
      (map ({ name, value }: { ${name} = value; }))
      # => [
      #      { elispFileVersion = <function>; }
      #      { genericBuild = <function>; }
      #    ]
      mergeAttrsList
      # => { 
      #      elispFileVersion = <function>;
      #      genericBuild = <function>;
      #    }
      # 一般来讲，我们会这么写 import XXX.nix { inherit attr; };
      # 这里相当于最后得到一个传入 XXX.nix 的一个参数集
    ]
  );  # Nix 中 pipe 的写法是 pipe <初始对象> [ <函数1> <函数2> ... ]
in pipe packagesDir [                            # => ./. (当前包目录)
  readDir                                        # => { "package1" = "directory"; "package2" = "directory"; "default.nix" = "regular"; ... }
  attrNames                                      # => [ "package1" "package2" "default.nix" ... ]
  (remove "default.nix")                         # => [ "package1" "package2" ... ]
  (map (dir: { "${dir}" = importFile dir; }))    # => [ { "package1" = <derivation>; } { "package2" = <derivation>; } ... ]
  mergeAttrsList                                 # => { "package1" = <derivation>; "package2" = <derivation>; ... }
]
