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
	    ;; 在mac中使用Command key作为meta
	    (setq mac-option-key-is-meta nil
	    	mac-command-key-is-meta t
	    	mac-command-modifier 'meta
	    	mac-option-modifier 'none)
	    
	    ;; 便于使用mac的JIS日语键盘
	    (global-set-key (kbd "C-¥") 'toggle-input-method)
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
	    (require 'hl-line)
	    (defun global-hl-line-timer-function ()
	      ;; 一定时间后后高亮所在行
	      (global-hl-line-unhighlight-all)
	      (let ((global-hl-line-mode t))
	        (global-hl-line-highlight)))
	    (setq global-hl-line-timer
	          ;; 30s后高亮所在行
	          (run-with-idle-timer 90.00 t 'global-hl-line-timer-function))
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
	    		     (font-height (if (> screen-height 900) 230 130))  ;; 根据屏幕高度调整
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
	    (with-eval-after-load 'pyim
	      ;; 基本设置
	      (setq default-input-method "pyim")
	      (setq pyim-dcache-directory "~/.emacs.d/.cache/pyim/dcache/")
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
	      '("emacs-lisp" "python" "C" "shell" "js" "clojure" "css" "nix"
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
	      ;; (require 'org-tempo)               ; 允许<Tab补齐org插入环境
	      )
	    (add-hook 'org-mode-hook #'visual-line-mode)
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
	    (setq exec-path (append exec-path '("${pkgs.python3}/bin/python3")))
	    
	    (add-to-list 'load-path (concat user-emacs-directory "site-lisp/emacs-application-framework/"))
	    (require 'eaf)
	    (require 'eaf-browser)
	    (require 'eaf-pdf-viewer)
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

		      # emacs 和包
		      emacsWithPackages = pkgs.emacs30-gtk3.pkgs.withPackages (epkgs:
		  
		  (with epkgs; [
		    vundo
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
		    meow
		    meow-tree-sitter
		    nix-mode
		  ])
	  );
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
	      
	      TARGET_DIR="$EMACS_DIR/site-lisp/emacs-application-framework/"
	      
	      if [ -d "$TARGET_DIR" ]; then
	          echo "目标目录已存在，跳过下载过程。"
	          cd $TARGET_DIR
	          ${pkgs.python3}/bin/python ./install-eaf.py --app-drop-local-edit -i browser pdf-viewer
	      else
	          ${pkgs.git}/bin/git clone --depth=1 -b master https://github.com/emacs-eaf/emacs-application-framework.git $TARGET_DIR
	          cd $TARGET_DIR
	          chmod +x ./install-eaf.py
	          ${pkgs.python3}/bin/python ./install-eaf.py --app-drop-local-edit -i browser pdf-viewer
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
