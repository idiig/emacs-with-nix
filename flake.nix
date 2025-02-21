{
  description = "idiig's Emacs Configuration with Nix Flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.11";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
  };

  outputs = { self, nixpkgs, flake-utils, emacs-overlay }:
    flake-utils.lib.eachDefaultSystem (system:
      let
	pkgs = import nixpkgs {
	  inherit system;
	  overlays = [ emacs-overlay.overlay ];
	};

	# 主配置文件
	emacsConfig = pkgs.writeText "init.el" ''
	  (add-hook 'after-init-hook
	  	  (lambda ()
	  	    (let ((font-path "${pkgs.sarasa-gothic}/share/fonts/truetype/Sarasa-Regular.ttc"))
	  	      (if (file-exists-p font-path)
	  		  (set-fontset-font t 'han (font-spec :file font-path))
	  		(message "字体文件不存在: %s" font-path)))))
	  (global-set-key (kbd "C-x j") 'skk-mode)
	  
	  (with-eval-after-load 'ddskk
	    (setq skk-server-inhibit-startup-server nil)
	    (setq skk-server-host "localhost")
	    (setq skk-server-portnum 55100)
	    (setq skk-share-private-jisyo t)
	  
	    ;; 候补显示设置
	    (setq skk-show-inline t)
	    (setq skk-show-tooltip t)
	    (setq skk-show-candidates-always-pop-to-buffer t)
	    (setq skk-henkan-show-candidates-rows 2)
	  
	    ;; 行为设置
	    (setq skk-egg-like-newline t)
	    (setq skk-delete-implies-kakutei nil)
	    (setq skk-use-look t)
	    (setq skk-auto-insert-paren t)
	    (setq skk-henkan-strict-okuri-precedence t)
	  
	    ;; 加载额外功能
	    (require 'skk-hint)
	    (add-hook 'skk-load-hook
	  	    (lambda ()
	  	      (require 'context-skk)))
	  
	    ;; 片假名转换设置
	    (setq skk-search-katakana 'jisx0201-kana))
	  
	  ;; (require 'ddskk nil t)
	  (with-eval-after-load 'pyim
	    ;; 基本设置
	    (setq default-input-method "pyim")
	    (setq pyim-dcache-directory "~/emacs-config/.cache/pyim/dcache/")
	    ;; 按键绑定
	    (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
	    ;; 输入法设置
	    (setq pyim-default-scheme 'quanpin)
	    ;; 启用搜索功能
	    (pyim-isearch-mode 1)
	    ;; 选词框设置
	    (setq pyim-page-tooltip 'popup)
	    (setq pyim-page-length 5)
	    ;; 加载并启用基础词库
	    (require 'pyim-basedict)
	    (pyim-basedict-enable))
	  
	  ;; ;; 加载 pyim 包
	  ;; (require 'pyim nil t)
	  
	  ;; diminish 设置 (如果使用 diminish)
	  (with-eval-after-load 'diminish
	    (diminish 'pyim-isearch-mode))
	  ;; 确保在 orderless 加载后再加载这些配置
	  (with-eval-after-load 'orderless
	    ;; 拼音检索字符串功能
	    (defun zh-orderless-regexp (orig_func component)
	      (call-interactively #'pyim-activate)
	      (call-interactively #'pyim-deactivate)
	      (let ((result (funcall orig_func component)))
	        (pyim-cregexp-build result)))
	    (advice-add 'orderless-regexp :around #'zh-orderless-regexp))
	  (add-to-list 'exec-path "${pkgs.git}/bin")
	  (require 'magit)
	  (with-eval-after-load 'org
	    (setq org-support-shift-select 2))
	  
	'';

	# early-init 配置文件
	emacsEarlyInitConfig = pkgs.writeText "early-init.el" ''
	  (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
	  
	  (load (concat user-emacs-directory "init.el"))
	  
	  (setq gc-cons-threshold 10485760
	        gc-cons-percentage 0.1)
	'';

  # emacs 和包
	emacsWithPackages = pkgs.emacs.pkgs.withPackages (epkgs: (with epkgs; [
	  ddskk
	  # (pkgs.emacsPackages.pyim.overrideAttrs (old: {
	  #     nativeComp = false;
	  # }))
	  pyim
	  pyim-basedict
	  magit
	  gptel
	]));

  # 输出配置到 .emacs.d
	  autoExportConfig = pkgs.writeShellScriptBin "auto-export-config" ''
	  #!/usr/bin/env bash
	  set -e

	  # 导出配置到 .emacs.d
	  mkdir -p "$HOME/.emacs.d"
	  ${pkgs.rsync}/bin/rsync ${emacsConfig} "$HOME/.emacs.d/init.el"
	  ${pkgs.rsync}/bin/rsync ${emacsEarlyInitConfig} "$HOME/.emacs.d/early-init.el"

	  echo "Emacs配置已同步到 $HOME/.emacs.d/"

	  # 启动 Emacs
	  exec ${emacsWithPackages}/bin/emacs "$@"
	'';
      in {
	packages = {
	  emacs = emacsWithPackages;
	  default = autoExportConfig;
    export-config = autoExportConfig;
	};

      }
    );
}
