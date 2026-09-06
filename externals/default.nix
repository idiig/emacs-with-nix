{ inputs, pkgs, emacsPackages }: let
  inherit (builtins) readDir;
  inherit (pkgs) runCommand;
  inherit (pkgs.lib) attrNames attrsToList filter functionArgs hasAttr mergeAttrsList pipe readFile remove;
  packagesDir = ./.;
  packageSources = inputs // {
    # nano = inputs.nano-emacs;
    # pyim-skk-style.el/skk-style-completion-framework.el 都在同一个
    # skk-style-completion-framework 仓库里，这个别名让
    # externals/pyim-skk-style/default.nix 也能拿到同一个 flake input。
    pyim-skk-style = inputs.skk-style-completion-framework;
  };
  importFile = dir: let
    packageFunction = import "${packagesDir}/${dir}";
  in emacsPackages.callPackage packageFunction (
    pipe ({
      # 大多数外部包只需要 package_src 加上面几个版本/构建辅助函数就够
      # 打包；这里额外注入 emacsPackages 本身，是给像
      # skk-style-completion-framework 这种自己就是 flake、直接暴露
      # `lib.mkXxxPackage = { emacsPackages }: ...` 的外部包用的——它
      # 们的 default.nix 不需要重新写一遍 melpaBuild，只要把
      # emacsPackages 转发回 package_src (这里是那个 flake 自己的
      # outputs) 自己的打包函数即可。
      inherit emacsPackages;
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
