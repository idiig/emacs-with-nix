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
(setq ring-bell-function 'ignore)
(defalias 'yes-or-no-p 'y-or-n-p)
(defun backward-kill-word-or-region (&optional arg)
  (interactive "p")
  (if (region-active-p)
      (call-interactively #'kill-region)
    (backward-kill-word arg)))
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop)
(customize-set-variable 'display-buffer-base-action
  '((display-buffer-reuse-window display-buffer-same-window)
    (reusable-frames . t)))
(defadvice split-window (after split-window-after activate)
  (other-window 1))
(require 'ctrlf)
(ctrlf-mode +1)
(add-hook 'after-init-hook
	  (lambda ()
	    (let ((my-font-height 130)
		  (my-font "Sarasa Mono SC"))
	      (set-face-attribute 'default nil :family my-font :height my-font-height))))

;; 工具栏，菜单保持默认字体
(set-face-attribute 'menu nil :inherit 'unspecified)
(set-face-attribute 'tool-bar nil :inherit 'unspecified)
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
  (setq pyim-dcache-directory "~/.emacs.d/.cache/pyim/dcache/")
  ;; C-return 把当前选中的位置转换为正则表达
  (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
  ;; 输入法设置为全拼
  (setq pyim-default-scheme 'quanpin)
  ;; 启用搜索功能
  (pyim-isearch-mode 1)
  ;; 选词框设置
  (setq pyim-page-tooltip 'popup)
  (setq pyim-page-length 5)
  ;; 加载并启用基础词库
  (require 'pyim-basedict)
  (pyim-basedict-enable))

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
(defvar idiig/language-list
  '("emacs-lisp" "python" "C" "sh" "js" "clojure" "css" "nix"
    "dot" "gnuplot" "R" "sql" "awk" "haskell" "latex" "lisp"
    "org" "julia" "scheme" "sqlite")
  "支持的编程语言列表。")
(with-eval-after-load 'org
  (defun idiig/org-insert-structure-template-src-advice (orig-fun type)
    "Advice for org-insert-structure-template to handle src blocks."
    (if (string= type "src")  ; 判断条件为 "src"
	(let ((selected-type (ido-completing-read "Source code type: " idiig/language-list)))
	  (funcall orig-fun (format "src %s" selected-type)))
      (funcall orig-fun type)))

  (advice-add 'org-insert-structure-template :around #'idiig/org-insert-structure-template-src-advice))
(defun idiig/load-org-babel-languages ()
  "根据 `idiig/language-list` 启用 `org-babel` 语言。"
  (let ((languages '()))
    (dolist (lang idiig/language-list)
      (push (cons (intern lang) t) languages)) ;; 将字符串转换为符号
    (org-babel-do-load-languages 'org-babel-load-languages languages)))

(add-hook 'org-mode-hook #'idiig/load-org-babel-languages)
(with-eval-after-load 'org
  (setq org-support-shift-select 2)  ; 允许shift用于选择
  (require 'org-tempo)               ; 允许<Tab补齐org插入环境
  )

(add-to-list 'exec-path "${pkgs.aider-chat}/bin")
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
	  meow
	  ctrlf
	  ddskk
	  # (pkgs.emacsPackages.pyim.overrideAttrs (old: {
	  #     nativeComp = false;
	  # }))
	  pyim
	  pyim-basedict
	  magit
	  ob-nix
	  gptel
	  # aider
	      ]));

	      # 输出配置到 .emacs.d
	      autoExportConfig = pkgs.writeShellScriptBin "auto-export-config" ''
	  #!/usr/bin/env bash
	  set -e

	  # 导出配置到 .emacs.d
	  mkdir -p "$HOME/.emacs.d"
	  ${pkgs.rsync}/bin/rsync ${emacsConfig} "$HOME/.emacs.d/init.el"
	  ${pkgs.rsync}/bin/rsync ${emacsEarlyInitConfig} "$HOME/.emacs.d/early-init.el"

    # 路径
    mkdir -p "$HOME/.local/share/fonts/truetype/"
    ${pkgs.rsync}/bin/rsync -av ${pkgs.sarasa-gothic}/share/fonts/truetype/ "$HOME/.local/share/fonts/sarasa-gothic/"
    fc-cache -f -v ~/.local/share/fonts/

	  # 更新Emacs路径
    sed -i '/^alias emacs=/d' "$HOME/.bashrc"
    echo 'alias emacs='"${emacsWithPackages}/bin/emacs" >> "$HOME/.bashrc"
    source "$HOME/.bashrc"

	  echo "Emacs配置已同步到 $HOME/.emacs.d/"
	'';
      in {
	      packages = {
		      default = autoExportConfig;
	      };

      }
    );
}
