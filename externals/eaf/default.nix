{ package_src
, elispFileVersion
, lib
, stdenv
, writeText
, melpaBuild
, elpa2nix
, melpa2nix
, makeWrapper

, python3
, qt6

, withSwayWMSupport ? stdenv.isLinux
, jq ? null

, withUnitySupport ? stdenv.isLinux
, xdotool ? null

, withX11Support ? true
, wmctrl ? null
}: let
  inherit (lib) readFile;
  
  python = python3.withPackages (pkgs: [
    pkgs.easyocr
    pkgs.epc
    pkgs.lxml
    pkgs.pygetwindow
    pkgs.pyqt6
    pkgs.pyqt6-sip
    pkgs.pyqt6-webengine
    pkgs.qrcode
    pkgs.requests
    pkgs.sexpdata
  ]);
in (melpaBuild (finalAttrs: {
  pname = "eaf";
  version = elispFileVersion "${finalAttrs.src}/eaf.el";
  src = package_src;

  # 在这里添加makeWrapper作为构建依赖
  nativeBuildInputs = [ makeWrapper ];

  patchPhase = ''
    runHook prePatch

    sed -i s#'defcustom eaf-python-command .*'#'defcustom eaf-python-command "${python.interpreter}"'# eaf.el
    ${if withSwayWMSupport then "substituteInPlace eaf.el --replace jq ${jq}/bin/jq" else ""}
    ${if withUnitySupport then "substituteInPlace eaf.el --replace xdotool ${xdotool}/bin/xdotool" else ""}
    ${if withX11Support then "substituteInPlace eaf.el --replace wmctrl ${wmctrl}/bin/wmctrl" else ""}

    mv core/eaf-epc.el .
    mv extension/* .

    runHook postPatch
  '';

  elpa2nix = writeText "elpa2nix.el" ''
    ${readFile elpa2nix}
    (defun byte-recompile-directory (&rest _))
  '';
  melpa2nix = writeText "melpa2nix.el" ''
    ${readFile melpa2nix}
    (defun byte-recompile-directory (&rest _))
  '';
})).overrideAttrs (old: {
  # 确保nativeBuildInputs被正确继承并添加makeWrapper
  nativeBuildInputs = (old.nativeBuildInputs or []) ++ [ makeWrapper ];
  
  # Override genericBuild's postInstall
  postInstall = ''
    DST=$out/share/emacs/site-lisp/elpa/$ename-$melpaVersion/

    mv eaf.py $DST
    mv applications.json $DST
    mv core $DST

    # 创建包装脚本目录
    mkdir -p $DST/bin

    # 直接创建包装脚本，使用PyQt6模块路径下的Qt库
    QT_LIB_PATH=$(${python}/bin/python -c "import os, PyQt6; print(os.path.join(os.path.dirname(PyQt6.__file__), 'Qt6/lib'))" 2>/dev/null)
    QT_PLUGIN_PATH=$(${python}/bin/python -c "import os, PyQt6; print(os.path.join(os.path.dirname(PyQt6.__file__), 'Qt6/plugins'))" 2>/dev/null)

   # 如果找不到常规路径，尝试其他可能的位置
   if [ ! -d "$QT_LIB_PATH" ] || [ ! -d "$QT_PLUGIN_PATH" ]; then
     QT_BASE_DIR=$(${python}/bin/python -c "import os, sys, PyQt6; print(os.path.normpath(os.path.join(os.path.dirname(PyQt6.__file__), '..', '..')))" 2>/dev/null)
     QT_LIB_PATH="$QT_BASE_DIR/lib"
     QT_PLUGIN_PATH="$QT_BASE_DIR/share/qt6/plugins"
   fi

   # 创建Python包装脚本
   makeWrapper ${python}/bin/python $DST/bin/eaf-python \
     --set LD_LIBRARY_PATH "$QT_LIB_PATH" \
     --set QT_PLUGIN_PATH "$QT_PLUGIN_PATH" \
     --set QT_QPA_PLATFORM_PLUGIN_PATH "$QT_PLUGIN_PATH/platforms" \
     --unset QT_XCB_GL_INTEGRATION \
     --unset XDG_DATA_DIRS

   # 修改eaf.el使用包装的Python
   sed -i s#'defcustom eaf-python-command .*'#'defcustom eaf-python-command "'$DST/bin/eaf-python'"'# $DST/eaf.el
'';
})
