{
  description = "idiig's Emacs Configuration with Nix Flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    eaf = {
      url = "github:emacs-eaf/emacs-application-framework";
      flake = false;
    };
    eaf-browser = {
      url = "github:emacs-eaf/eaf-browser";
      flake = false;
    };
    eaf-pdf-viewer = {
      url = "github:emacs-eaf/eaf-pdf-viewer";
      flake = false;
    };
  };

  outputs = inputs@{ self, nixpkgs, flake-utils, emacs-overlay, ... }:
    let
      # 用于安装非 nixpkgs 元的外部包的函数
      mkPackages = pkgs: emacsPackages: import ./externals {
        inherit inputs pkgs emacsPackages;
      };
    in
      flake-utils.lib.eachDefaultSystem (system:
	      let
          
		      pkgs = import nixpkgs {
			      inherit system;
			      overlays = [ emacs-overlay.overlay ];
		      };

		      # 主配置文件
		      emacsConfig = pkgs.writeText "init.el" ''
	    ;; 在mac中使用Command key作为meta
	    (setq mac-option-key-is-meta nil
	    	mac-command-key-is-meta t
	    	mac-command-modifier 'meta
	    	mac-option-modifier 'none)
	    
	    ;; 便于使用mac的JIS日语键盘
	    (global-set-key (kbd "C-¥") 'toggle-input-method)
	    (require 'use-package)
	    (require 'diminish)
	    (setq ring-bell-function 'ignore)
	    (defalias 'yes-or-no-p 'y-or-n-p)
	    (setq enable-recursive-minibuffers t)
	    ;; 基础设置
	    (tool-bar-mode -1) ;; 关闭工具栏
	    (scroll-bar-mode -1) ;; 关闭文件滑动控件
	    (setq inhibit-splash-screen 1) ;; 关闭启动帮助画面
	    (setq initial-frame-alist (quote ((fullscreen . maximized)))) ;; 全屏
	    (setq initial-scratch-message nil) ;; 关闭scratch message
	    (setq inhibit-startup-message t) ;; 关闭启动信息
	    (setq frame-title-format
	          ;; 窗口显示文件路径/buffer名
	          '("" " idiig - "
	            (:eval (if (buffer-file-name)
	                       (abbreviate-file-name (buffer-file-name)) "%b"))))
	    (setq ns-use-proxy-icon nil)  ;; 删除frame icon
	    (require-theme 'modus-themes)
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
	    (defadvice split-window-below (after split-window-below-and-switch activate)
	      "切换到新分割的窗口"
	      (when (called-interactively-p 'any)
	        (other-window 1)))
	    
	    (defadvice split-window-right (after split-window-right-and-switch activate)
	      "切换到新分割的窗口"
	      (when (called-interactively-p 'any)
	        (other-window 1)))
	    (global-set-key (kbd "C-x V") 'shrink-window)
	    
	    (defun idiig/window-adjust (orig-fun &rest args)
	      "使用 Emacs 风格按键 (^, V, {, }, +) 持续调整窗口大小。"
	      (let* ((ev last-command-event)
	    	   (echo-keystrokes nil))
	        ;; 执行初始调整
	        (apply orig-fun args)
	    
	        ;; 设置 transient map
	        (let ((delta (car args))) 
	    	(set-transient-map
	    	 (let ((map (make-sparse-keymap)))
	    	   ;; 垂直调整
	    	   (define-key map (kbd "^")
	    		       `(lambda () (interactive) (enlarge-window ,delta nil)))
	    	   (define-key map (kbd "V")
	    		       `(lambda () (interactive) (shrink-window ,delta nil)))
	    
	    	   ;; 水平调整
	    	   (define-key map (kbd "{")
	    		       `(lambda () (interactive) (shrink-window ,delta t)))
	    	   (define-key map (kbd "}")
	    		       `(lambda () (interactive) (enlarge-window ,delta t)))
	    
	    	   ;; 平衡窗口
	    	   (define-key map (kbd "+")
	    		       (lambda () (interactive) (balance-windows)))
	    	   map)
	    	 nil nil
	    	 "Use %k for further adjustment"))))
	    
	    ;; ;; 如果需要移除 advice:
	    ;; (advice-remove 'enlarge-window #'idiig/window-adjust)
	    ;; (advice-remove 'shrink-window #'idiig/window-adjust)
	    ;; (advice-remove 'enlarge-window-horizontally #'idiig/window-adjust)
	    ;; (advice-remove 'shrink-window-horizontally #'idiig/window-adjust)
	    
	    ;; 添加 advice
	    (advice-add 'enlarge-window :around #'idiig/window-adjust)
	    (advice-add 'shrink-window :around #'idiig/window-adjust)
	    (advice-add 'enlarge-window-horizontally :around #'idiig/window-adjust)
	    (advice-add 'shrink-window-horizontally :around #'idiig/window-adjust)
	    (global-set-key (kbd "C-x u") 'vundo)
	    (require 'ctrlf)
	    (ctrlf-mode +1)
	    (add-hook 'after-init-hook
	    	    (lambda ()
	    	      (let* ((screen-height (display-pixel-height))
	    		     (font-height (if (and
	    				       (< screen-height 1150)
	    				       (> screen-height 1200)) 230 130))  ;; 根据屏幕高度调整
	    		     (minibuffer-font-height (- font-height 0))
	    		     (my-font "Sarasa Mono SC"))
	    		(set-face-attribute 'default nil :family my-font :height font-height)
	    		;; 设置 mode-line 字体
	    		(set-face-attribute 'mode-line nil :family my-font :height font-height)
	    		(set-face-attribute 'mode-line-inactive nil :family my-font :height font-height)
	    		;; 设置 minibuffer 字体
	    		(set-face-attribute 'minibuffer-prompt nil :family my-font :height minibuffer-font-height))))
	    
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
	    (use-package pyim
	      :diminish pyim-isearch-mode
	      :command
	      (toggle-input-method)
	      :custom
	      (default-input-method "pyim")
	      (pyim-dcache-directory (concat user-emacs-directory "pyim/dcache"))
	      (pyim-default-scheme 'quanpin)
	      (pyim-page-tooltip 'popup)
	      (pyim-page-length 4)
	      :config
	      ;; 启用搜索功能
	      (pyim-isearch-mode 1)
	      ;; 加载并启用基础词库
	      (require 'pyim-basedict)
	      (pyim-basedict-enable))
	    (with-eval-after-load 'pyim
	      (require 'pyim-cstring-utils)
	    
	      ;; C-return 把当前选中的位置转换为正则表达
	      (define-key minibuffer-local-map (kbd "C-<return>") 'pyim-cregexp-convert-at-point)
	    
	      (defvar idiig/pyim-region-enabled nil
	        "记录pyim区域功能是否启用的状态变量。")
	    
	      (defun idiig/toggle-pyim-region ()
	        "切换pyim的单词移动功能。
	    当启用时，会将forward-word和backward-word重映射为pyim的相应函数；
	    当禁用时，会恢复原来的映射。"
	        (interactive)
	        (call-interactively #'pyim-activate)
	        (call-interactively #'pyim-deactivate)
	        (if idiig/pyim-region-enabled
	    	  (progn
	    	    (idiig/disable-pyim-region)
	    	    (setq idiig/pyim-region-enabled nil)
	    	    (message "已禁用pyim区域功能"))
	    	(progn
	    	  (idiig/enable-pyim-region)
	    	  (setq idiig/pyim-region-enabled t)
	    	  (message "已启用pyim区域功能"))))
	    
	      (defun idiig/enable-pyim-region (&rest _)
	        "启用pyim的单词移动建议。"
	        (global-set-key [remap forward-word] 'pyim-forward-word)
	        (global-set-key [remap backward-word] 'pyim-backward-word))
	    
	      (defun idiig/disable-pyim-region (&rest _)
	        "禁用pyim的单词移动建议。"
	        (global-unset-key [remap forward-word])
	        (global-unset-key [remap backward-word]))
	    
	      ;; ;; 挂钩到 pyim 的启用/禁用钩子上
	      ;; (advice-remove 'pyim-deactivate #'idiig/disable-pyim-region)
	      ;; (advice-remove 'pyim-activate #'idiig/enable-pyim-region)
	      ;; (advice-add 'pyim-deactivate :after #'idiig/disable-pyim-region)
	      (advice-add 'pyim-activate :after #'idiig/enable-pyim-region))
	    ;; 确保在 orderless 和 pyim 加载后再加载这些配置
	    (with-eval-after-load '(orderless pyim)
	      ;; 拼音检索字符串功能
	      (defun zh-orderless-regexp (orig_func component)
	        (call-interactively #'pyim-activate)
	        (call-interactively #'pyim-deactivate)
	        (let ((result (funcall orig_func component)))
	    	(pyim-cregexp-build result)))
	      (advice-add 'orderless-regexp :around #'zh-orderless-regexp))
	    (use-package magit
	      :bind ("C-x g" . magit-status)
	      :commands magit-status
	      :init
	      ;; 使用nix路径中的git
	      (add-to-list 'exec-path "${pkgs.git}/bin"))
	    ;; TODO: 这里未来需要改成在每个语言的设定的节点push进来
	    (defvar idiig/language-list
	      '("emacs-lisp" "python" "C" "shell" "js" "clojure" "css" "nix"
	        "dot" "gnuplot" "R" "sql" "awk" "haskell" "latex" "lisp"
	        "org" "julia" "scheme" "sqlite")
	      "支持的编程语言列表。")
	    
	    (defun idiig/run-prog-mode-hooks ()
	      "Runs `prog-mode-hook'. 针对一些本该为编程语言又没自动加载prog mode的语言hook.
	    如：(add-hook 'python-hook 'idiig/run-prog-mode-hooks)
	    "
	      (run-hooks 'prog-mode-hook))
	    
	    ;; 为每种语言添加钩子
	    (add-hook 'after-init-hook
	     (dolist (lang idiig/language-list)
	       (let ((mode-hook (intern (concat lang "-mode-hook"))))
	         (add-hook mode-hook 'idiig/run-prog-mode-hooks))))
	    (defmacro idiig//setup-nix-lsp-server (language server-name executable-path &optional lib-path)
	      "配置 Nix 环境下的 LSP 服务器。
	    LANGUAGE 是语言名称，如 'python'。
	    SERVER-NAME 是服务器名称，如 'basedpyright'。
	    EXECUTABLE-PATH 是服务器可执行文件的路径。
	    LIB-PATH 是可选的库路径，添加到 LD_LIBRARY_PATH。"
	      `(with-eval-after-load 'lsp-bridge
	         ;; 设置 LSP 服务器
	         (setq ,(intern (format "lsp-bridge-%s-lsp-server" language)) ,server-name)
	         
	         ;; 添加可执行文件路径到 exec-path
	         ,(when executable-path
	            `(add-to-list 'exec-path ,executable-path))
	         
	         ;; 添加库路径到 LD_LIBRARY_PATH
	         ,(when lib-path
	            `(setenv "LD_LIBRARY_PATH" 
	                    (concat ,lib-path ":" 
	                            (or (getenv "LD_LIBRARY_PATH") ""))))))
	    (use-package lsp-bridge
	      :defer t
	      :bind
	      (:map acm-mode-map
	            ("C-j" . acm-select-next)
	            ("C-k" . acm-select-prev))
	      :custom
	      (acm-enable-yas nil)   ; 补全不包括 Yasnippet
	      (acm-enable-doc nil)   ; 不自动显示函数等文档
	      (lsp-bridge-org-babel-lang-list idiig/language-list)  ; org支持的代码也使用桥
	      (acm-enable-icon nil)  ; 不显示图标
	      :hook 
	      (prog-mode . (lambda ()
	    		 (lsp-bridge-mode)))
	      :init
	      ;; 这里是为了让语言服务器找到正确的版本的 libstdc++.so.6 库
	      (setenv "LD_LIBRARY_PATH" 
	                (concat "${pkgs.stdenv.cc.cc.lib}/lib:" 
	                        (or (getenv "LD_LIBRARY_PATH") ""))))
	    (use-package treesit-auto
	      :custom
	      (treesit-auto-install 'prompt)   ; 设置安装 tree-sitter 语法时提示用户确认
	      :hook
	      (prog-mode . treesit-auto-mode)    ; 在所有编程模式下自动启用 treesit-auto-mode
	      :config
	      (treesit-auto-add-to-auto-mode-alist 'all))  ; 将所有已知的 tree-sitter 模式添加到自动模式列表中
	    ;; (defvar idiig/snippet-dir (concat user-emacs-directory "snippets"))
	    
	    (use-package yasnippet
	      :defer t
	      ;; :diminish
	      :hook
	      (prog-mode . yas-minor-mode)
	      :init
	      ;; (setq yas-snippet-dirs <path/to/snippets>)
	      ;; (push idiig/snippet-dir yas-snippet-dirs)
	      :config
	      (yas-reload-all))
	    (idiig//setup-nix-lsp-server 
	     "nix" 
	     "nixd" 
	     "${pkgs.nixd}/bin" 
	     nil)
	    (idiig//setup-nix-lsp-server 
	     "python" 
	     "basedpyright" 
	     "${pkgs.basedpyright}/bin" 
	     "${pkgs.stdenv.cc.cc.lib}/lib")
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
	      (setq org-support-shift-select 2))
	    (add-hook 'org-mode-hook #'visual-line-mode)
	    (with-eval-after-load 'org
	      (setq org-display-remote-inline-images t))
	    (add-hook 'org-mode-hook
	              (lambda ()
	                (when (string-match-p "\\.ai\\.org\\'" (buffer-file-name))
	                  (gptel-mode 1))))
	    (add-to-list 'exec-path "${pkgs.aider-chat}/bin")
	    ;; (defalias 'meow-visit #'ctrlf-forward-default) ; 需要ctrlf
	    
	    ;; https://github.com/meow-edit/meow/blob/master/KEYBINDING_QWERTY.org
	    (defun meow-setup ()
	      (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
	      (meow-motion-define-key
	       '("j" . meow-next)
	       '("k" . meow-prev)
	       '("<escape>" . ignore))
	      (meow-leader-define-key
	       ;; Use SPC (0-9) for digit arguments.
	       '("1" . meow-digit-argument)
	       '("2" . meow-digit-argument)
	       '("3" . meow-digit-argument)
	       '("4" . meow-digit-argument)
	       '("5" . meow-digit-argument)
	       '("6" . meow-digit-argument)
	       '("7" . meow-digit-argument)
	       '("8" . meow-digit-argument)
	       '("9" . meow-digit-argument)
	       '("0" . meow-digit-argument)
	       '("/" . meow-keypad-describe-key)
	       '("?" . meow-cheatsheet))
	      (meow-normal-define-key
	       '("0" . meow-expand-0)
	       '("9" . meow-expand-9)
	       '("8" . meow-expand-8)
	       '("7" . meow-expand-7)
	       '("6" . meow-expand-6)
	       '("5" . meow-expand-5)
	       '("4" . meow-expand-4)
	       '("3" . meow-expand-3)
	       '("2" . meow-expand-2)
	       '("1" . meow-expand-1)
	       '("-" . negative-argument)
	       '(";" . meow-reverse)
	       '("," . meow-inner-of-thing)
	       '("." . meow-bounds-of-thing)
	       '("[" . meow-beginning-of-thing)
	       '("]" . meow-end-of-thing)
	       '("a" . meow-append)
	       '("A" . meow-open-below)
	       '("b" . meow-back-word)
	       '("B" . meow-back-symbol)
	       '("c" . meow-change)
	       '("d" . meow-delete)
	       '("D" . meow-backward-delete)
	       '("e" . meow-next-word)
	       '("E" . meow-next-symbol)
	       '("f" . meow-find)
	       '("g" . meow-cancel-selection)
	       '("G" . meow-grab)
	       '("h" . meow-left)
	       '("H" . meow-left-expand)
	       '("i" . meow-insert)
	       '("I" . meow-open-above)
	       '("j" . meow-next)
	       '("J" . meow-next-expand)
	       '("k" . meow-prev)
	       '("K" . meow-prev-expand)
	       '("l" . meow-right)
	       '("L" . meow-right-expand)
	       '("m" . meow-join)
	       '("n" . meow-search)
	       '("o" . meow-block)
	       '("O" . meow-to-block)
	       '("p" . meow-yank)
	       '("q" . meow-quit)
	       '("Q" . meow-goto-line)
	       '("r" . meow-replace)
	       '("R" . meow-swap-grab)
	       '("s" . meow-kill)
	       '("t" . meow-till)
	       '("u" . meow-undo)
	       '("U" . meow-undo-in-selection)
	       '("v" . meow-visit)
	       '("w" . meow-mark-word)
	       '("W" . meow-mark-symbol)
	       '("x" . meow-line)
	       '("X" . meow-goto-line)
	       '("y" . meow-save)
	       '("Y" . meow-sync-grab)
	       '("z" . meow-pop-selection)
	       '("'" . repeat)
	       '("<escape>" . ignore)))
	    
	    (require 'meow)
	    (meow-setup)
	    (meow-global-mode 1)
	    (require 'meow-tree-sitter)
	    (meow-tree-sitter-register-defaults)  
	    (defvar-local the-late-input-method nil)
	    (add-hook 'meow-insert-enter-hook
	    	    (lambda ()
	    	      (activate-input-method the-late-input-method)))
	    (add-hook 'meow-insert-exit-hook
	    	    (lambda ()
	    	      (setq the-late-input-method current-input-method)
	    	      (deactivate-input-method)))
	    ;; (defvar idiig/eaf-path (concat user-emacs-directory "site-lisp/emacs-application-framework"))
	    ;; (add-to-list 'load-path idiig/eaf-path)
	    ;; (setq eaf-python-command (concat idiig/eaf-path "/eaf/bin/python"))
	    
	    (require 'eaf)
	    (require 'eaf-browser)
	    ;; (require 'eaf-pdf-viewer)
	    
	    (setq eaf-webengine-default-zoom 2.0
	          eaf-browse-blank-page-url "https://kagi.com"
	          eaf-browser-auto-import-chrome-cookies nil   ; 非自动 cookies
	          eaf-browser-enable-autofill t                ; 自动填充密码
	          eaf-browser-enable-tampermonkey t            ; 使用油猴
	          )
	    '';

		      # early-init 配置文件
		      emacsEarlyInitConfig = pkgs.writeText "early-init.el" ''
	    ;; 增加 GC 阈值，加快启动
	    (setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)
	    
	    ;; 启动完成后恢复正常 GC 设定
	    (add-hook 'emacs-startup-hook
	    	    (lambda ()
	    	      (setq gc-cons-threshold 10485760
	    		    gc-cons-percentage 0.1)))
	    
	    ;; 禁用bidi，加速大文件
	    (setq-default bidi-display-reordering nil)
	    (setq bidi-inhibit-bpa t
	    	long-line-threshold 1000
	    	large-hscroll-threshold 1000
	    	syntax-wholeline-max 1000)
	    '';

          # 首先定义你的基础 Emacs
          emacs = pkgs.emacs30-gtk3;

          # 定义覆盖函数
          overrides = final: prev: mkPackages pkgs final;
          
          # 创建扩展的包集合并选择包
          emacsWithPackages = ((pkgs.emacsPackagesFor emacs).overrideScope overrides).withPackages (epkgs: with epkgs; [
            use-package
              diminish
            vundo
            ctrlf
            ddskk
            # (pkgs.emacsPackages.pyim.overrideAttrs (old: {
            #     nativeComp = false;
            # }))
            pyim
              pyim-basedict
            magit
            (lsp-bridge.override {
              # 指定使用 Python 3.11 而不是 3.12
              python3 = pkgs.python311;
            })
              markdown-mode
              yasnippet
            # treesit  # 目前 treesit 已经内置
            treesit-auto
            # yasnippet
            yasnippet-snippets
            nix-mode
            ob-nix
            gptel
            # aider
            meow
            meow-tree-sitter
            eaf
              eaf-browser
              # eaf-pdf-viewer
          ]);
          
	      in {
		      packages.default = pkgs.writeShellScriptBin "script" ''
	      #!/usr/bin/env bash
	      set -e

	      # 导出配置到 nix-emacs
	      EMACS_DIR="$HOME/nix-emacs"
	      mkdir -p "$EMACS_DIR"
	      ${pkgs.rsync}/bin/rsync ${emacsConfig} "$EMACS_DIR/init.el"
	      ${pkgs.rsync}/bin/rsync ${emacsEarlyInitConfig} "$EMACS_DIR/early-init.el"

	      # 路径
	      if [ "$(uname)" = "Darwin" ]; then
	        # macOS
	        mkdir -p "$HOME/Library/Fonts/"
	        ${pkgs.rsync}/bin/rsync -av ${pkgs.sarasa-gothic}/share/fonts/truetype/ "$HOME/Library/Fonts/"
	      else
	        # Assume Linux
	        mkdir -p "$HOME/.local/share/fonts/truetype/"
	        ${pkgs.rsync}/bin/rsync -av ${pkgs.sarasa-gothic}/share/fonts/truetype/ "$HOME/.local/share/fonts/sarasa-gothic/"
	        fc-cache -f -v ~/.local/share/fonts/
	      fi

	      # 更新 Emacs 路径（兼容 macOS 和 Linux）
        if sed --version 2>/dev/null | grep "(GNU sed)"; then
		      sed -i '/^alias ne=/d' "$HOME/.bashrc"
	      else
		      sed -i \"\" '/^alias ne=/d' "$HOME/.bashrc"
	      fi

	      echo "alias ne='${emacsWithPackages}/bin/emacs --init-dir \"$EMACS_DIR\"'" >> "$HOME/.bashrc"

	      # 提示用户手动 source 而不是直接执行，以避免 shell 继承问题
	      echo "请手动运行 'source ~/.bashrc' 以使 alias 生效"
	      echo "Emacs 配置已同步到 $EMACS_DIR"
	      '';  
	      });
}
