{ pkgs ? import <nixpkgs> {} }:

let
  # Python packages needed for EAF
  my-python-packages = python-packages: with python-packages; [
    pandas
    requests
    sexpdata
    tld
    pyqt6
    pyqt6-sip
    pyqt6-webengine
    epc
    lxml       # for eaf
    qrcode     # eaf-file-browser
    pysocks    # eaf-browser
    pymupdf    # eaf-pdf-viewer
    pypinyin   # eaf-file-manager
    psutil     # eaf-system-monitor
    retry      # eaf-markdown-previewer
    markdown
  ];
  python-with-packages = pkgs.python3.withPackages my-python-packages;
in
pkgs.mkShell {
  buildInputs = with pkgs; [
    
    # System dependencies
    git
    nodejs
    wmctrl
    xdotool
    aria    # for eaf-browser
    fd      # for eaf-file-manager

    # Python with required packages
    python-with-packages
    pkgs.qt6.qtbase
    pkgs.qt6.qtwebengine

    # OpenGL/VAAPI support
    libvdpau-va-gl
  ];

  # 环境变量设置
  shellHook = ''
    TARGET_DIR="$HOME/nix-emacs/site-lisp/emacs-application-framework"
    EAF_PYTHON_PATH="$TARGET_DIR/eaf"

    export QT_QPA_PLATFORM_PLUGIN_PATH="${pkgs.qt6.qtbase.outPath}/lib/qt-6/plugins"   

    if [ -d "$TARGET_DIR" ]; then
        echo "目标目录已存在，跳过下载过程。"
        cd $TARGET_DIR
        ./install-eaf.py --ignore-core-deps
    else
        git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git $TARGET_DIR
        cd $TARGET_DIR
        chmod +x ./install-eaf.py
        ./install-eaf.py  --ignore-core-deps
    fi

  '';
}
