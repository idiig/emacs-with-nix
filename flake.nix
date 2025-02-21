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
  emacsConfig = pkgs.writeText "default.el" ''
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
	  
	  (require 'ddskk nil t)
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
	  
	  ;; 加载 pyim 包
	  (require 'pyim nil t)
	  
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
	'';        
	      emacsWithConfig = pkgs.emacs.pkgs.withPackages (epkgs: (with epkgs; [
		(pkgs.runCommand "default.el" {} ''
		   mkdir -p $out/share/emacs/site-lisp
		   cp ${emacsConfig} $out/share/emacs/site-lisp/default.el
		'')
	  ddskk
	  # (pkgs.emacsPackages.pyim.overrideAttrs (old: {
	  #     nativeComp = false;
	  # }))
	  pyim
	  pyim-basedict
	  magit
	      ]));
      in {
	      packages = {
		      emacs = emacsWithConfig;
		      default = emacsWithConfig;  # 用于nix run
	      };
      }
    );
}
