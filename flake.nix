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
	    ;; 关闭警告声
	    (setq ring-bell-function 'ignore)
	    
	    ;; 确认使用y或n，而不是yes或no。
	    (defalias 'yes-or-no-p 'y-or-n-p)
	    
	    ;; 不自动生成备份文件
	    (setq make-backup-files nil)
	    
	    ;; 选中文字能被整体替换（与其他文本编辑器相同）
	    (delete-selection-mode 1)
	    
	    ;; 文件最后添加新行
	    (setq require-final-newline t)
	    
	    ;; 文件在外部更新时buffer更新
	    (global-auto-revert-mode 1)
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
	    
	    (defun idiig/window-adjust-advice (orig-fun &rest args)
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
	    	 ;; 最大化窗口
	    	 (define-key map (kbd "M")
	    		     (lambda () (interactive) (maximize-window)))
	    	 ;; 最小化窗口
	    	 (define-key map (kbd "m")
	    		     (lambda () (interactive) (minimize-window)))
	    	 map)
	           nil nil
	           "Use %k for further adjustment"))))
	    
	    ;; ;; 如果需要移除 advice:
	    ;; (advice-remove 'enlarge-window #'idiig/window-adjust-advice)
	    ;; (advice-remove 'shrink-window #'idiig/window-adjust-advice)
	    ;; (advice-remove 'enlarge-window-horizontally #'idiig/window-adjust-advice)
	    ;; (advice-remove 'shrink-window-horizontally #'idiig/window-adjust-advice)
	    
	    ;; 添加 advice
	    (advice-add 'enlarge-window :around #'idiig/window-adjust-advice)
	    (advice-add 'shrink-window :around #'idiig/window-adjust-advice)
	    (advice-add 'enlarge-window-horizontally :around #'idiig/window-adjust-advice)
	    (advice-add 'shrink-window-horizontally :around #'idiig/window-adjust-advice)
	    (advice-add 'maximize-window :around #'idiig/window-adjust-advice)
	    (advice-add 'minimize-window :around #'idiig/window-adjust-advice)
	    ;; 不存在文档时询问是否新建
	    (add-hook 'before-save-hook
	              (lambda ()
	                (when buffer-file-name
	                  (let ((dir (file-name-directory buffer-file-name)))
	                    (when (and (not (file-exists-p dir))
	                               (y-or-n-p (format "Directory %s does not exist. Create it?" dir)))
	                      (make-directory dir t))))))
	    
	    ;; 找文件时若无母文档则新建 
	    (defadvice find-file (before make-directory-maybe
	                                 (filename &optional wildcards) activate)
	      "Create parent directory if not exists while visiting file."
	      (unless (file-exists-p filename)
	        (let ((dir (file-name-directory filename)))
	          (when dir
	            (unless (file-exists-p dir)
	              (make-directory dir t))))))
	    (use-package recentf
	      :defer t
	      :commands
	      (consult-recent-file)
	      :init
	      (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
	            recentf-max-saved-items 500
	            recentf-max-menu-items 10)
	      (setq recentf-exclude
	            '("COMMIT_MSG"
	              "COMMIT_EDITMSG"
	              "github.*txt$"
	              "/tmp/"
	              "/sudo:"
	              "/TAGS$"
	              "/GTAGS$"
	              "/GRAGS$"
	              "/GPATH$"
	              "\\.mkv$"
	              "\\.mp[34]$"
	              "\\.avi$"
	              "\\.sub$"
	              "\\.srt$"
	              "\\.ass$"
	              ".*png$"
	              "Nutstore/org-files/"
	              "bookmarks"))
	      (setq recentf-max-saved-items 2048)
	      (recentf-mode 1))
	    
	    ;; cleanup recent files
	    (defun idiig/cleanup-recentf ()
	      (progn
	        (and (fboundp 'recentf-cleanup)
	             (recentf-cleanup))))
	    (add-hook 'kill-emacs-hook #'idiig/cleanup-recentf)
	    (use-package savehist
	      :init
	      (setq savehist-additional-variables
	            ;; search entries
	            '(search-ring regexp-search-ring)
	            ;; 每一分钟保存一次
	            savehist-autosave-interval 60
	            ;; keep the home clean
	            savehist-file (expand-file-name "savehist" user-emacs-directory))
	      (savehist-mode t))
	    (use-package bookmark
	      :init
	      (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
	            bookmark-save-flag 1))
	    (use-package saveplace
	      :init
	      (setq save-place-file (expand-file-name "place" user-emacs-directory))
	      (save-place-mode 1))
	    (use-package mwim
	      :bind
	      ("C-a" . mwim-beginning-of-code-or-line-or-comment)
	      ("C-e" . mwim-end-of-code-or-line)
	      :commands
	      (mwim-beginning-of-code-or-line-or-comment
	       mwim-end-of-code-or-line))
	    (use-package unfill
	      :bind
	      ("M-q" . unfill-toggle)
	      :commands
	      (unfill-toggle))
	    (use-package emacs
	      :init
	      (progn
	        ;; 为`completing-read-multiple'添加提示，比如[CRM<separator>]
	        (defun crm-indicator (args)
	          (cons (format "[CRM%s] %s"
	                        (replace-regexp-in-string
	                         "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
	                         crm-separator)
	                        (car args))
	                (cdr args)))
	        (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
	    
	        ;; 不允许鼠标出现在minibuffer的提示中
	        (setq minibuffer-prompt-properties
	              '(read-only t cursor-intangible t face minibuffer-prompt))
	        (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
	    
	        ;; 在emacs 28以后，非当前mode的指令都会被隐藏，vertico的指令也会隐藏
	        (setq read-extended-command-predicate
	              #'command-completion-default-include-p)
	    
	        ;; minibuffer可循环
	        (setq enable-recursive-minibuffers t)))
	    
	    ;; http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
	    ;; 使用鼠标时关闭minibuffer
	    (defun idiig/stop-using-minibuffer ()
	      "kill the minibuffer"
	      (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
	        (abort-recursive-edit)))
	    (add-hook 'mouse-leave-buffer-hook 'idiig/stop-using-minibuffer)
	    
	    (use-package vertico
	      :after consult
	      :custom
	      (vertico-count 9)
	      (vertico-cycle t)
	      :init
	      (vertico-mode))
	    (use-package orderless
	      :after
	      (consult)
	      :config
	      (setq search-default-mode t)
	      (defvar +orderless-dispatch-alist
	        '((?% . char-fold-to-regexp)
	          (?! . orderless-without-literal)
	          (?`. orderless-initialism)
	          (?= . orderless-literal)
	          (?~ . orderless-flex)))
	    
	      (defun +orderless--suffix-regexp ()
	        (if (and (boundp 'consult--tofu-char) (boundp 'consult--tofu-range))
	            (format "[%c-%c]*$"
	                    consult--tofu-char
	                    (+ consult--tofu-char consult--tofu-range -1))
	          "$"))
	      ;; Recognizes the following patterns:
	      ;; * ~flex flex~
	      ;; * =literal literal=
	      ;; * %char-fold char-fold%
	      ;; * `initialism initialism`
	      ;; * !without-literal without-literal!
	      ;; * .ext (file extension)
	      ;; * regexp$ (regexp matching at end)
	      (defun +orderless-dispatch (word _index _total)
	        (cond
	         ;; Ensure that $ works with Consult commands, which add disambiguation suffixes
	         ((string-suffix-p "$" word)
	          `(orderless-regexp . ,(concat (substring word 0 -1) (+orderless--suffix-regexp))))
	         ;; File extensions
	         ((and (or minibuffer-completing-file-name
	                   (derived-mode-p 'eshell-mode))
	               (string-match-p "\\`\\.." word))
	          `(orderless-regexp . ,(concat "\\." (substring word 1) (+orderless--suffix-regexp))))
	         ;; Ignore single !
	         ((equal "!" word) `(orderless-literal . ""))
	         ;; Prefix and suffix
	         ((if-let (x (assq (aref word 0) +orderless-dispatch-alist))
	              (cons (cdr x) (substring word 1))
	            (when-let (x (assq (aref word (1- (length word))) +orderless-dispatch-alist))
	              (cons (cdr x) (substring word 0 -1)))))))
	      
	      ;; Define orderless style with initialism by default ; add migemo feature for japanese
	      (orderless-define-completion-style +orderless-with-initialism
	        (orderless-matching-styles '(orderless-initialism
	                                     orderless-literal
	                                     orderless-regexp)))
	      
	      (setq completion-styles '(orderless basic)
	            completion-category-defaults nil
	            ;;; Enable partial-completion for files.
	            ;;; Either give orderless precedence or partial-completion.
	            ;;; Note that completion-category-overrides is not really an override,
	            ;;; but rather prepended to the default completion-styles.
	            ;; completion-category-overrides '((file (styles orderless partial-completion))) ;; orderless is tried first
	            completion-category-overrides '((file (styles partial-completion)) ;; partial-completion is tried first
	                                            (buffer (styles +orderless-with-initialism))
	                                            (consult-location (styles +orderless-with-initialism))
	                                            ;; enable initialism by default for symbols
	                                            (command (styles +orderless-with-initialism))
	                                            (variable (styles +orderless-with-initialism))
	                                            (symbol (styles +orderless-with-initialism)))
	            orderless-component-separator #'orderless-escapable-split-on-space ;; allow escaping space with backslash!
	            orderless-style-dispatchers '(+orderless-dispatch)))
	    (use-package marginalia
	      :after vertico
	      ;; 只在minibuffer启用快捷键
	      :bind (:map minibuffer-local-map ("M-A" . marginalia-cycle))
	      :init
	      (setq marginalia-align-offset 5)
	      :config
	      (marginalia-mode))
	    (use-package consult
	      :hook (after-init . (lambda () (require 'consult)))
	      :bind (([remap M-x] . execute-extended-command)
	             ([remap goto-line] . consult-goto-line)
	             ([remap switch-to-buffer] . consult-buffer)
	             ([remap find-file] . find-file)
	    	 ([remap imenu] . consult-imenu)
	             ("C-x C-r" . consult-recent-file)
	             ("C-c y" . consult-yasnippet)
	             ("C-c f" . consult-find)
	             ("C-s" . consult-line)
	             ("C-c o" . consult-file-externally)
	             ("C-x p f" . consult-ripgrep)
	             (:map minibuffer-local-map
	                   ("C-c h" . consult-history)
	                   ("C-s" . #'previous-history-element)))
	      :init
	      (defun idiig/consult-buffer-region-or-symbol ()
	        "consult-line当前字符或选中区域."
	        (interactive)
	        (let ((input (if (region-active-p)
	                         (buffer-substring-no-properties
	                          (region-beginning) (region-end))
	                       (thing-at-point 'symbol t))))
	          (consult-line input)))
	      (defun idiig/consult-project-region-or-symbol (&optional default-inputp)
	        "consult-ripgrep 当前字符或选中区域."
	        (interactive)
	        (let ((input (if (region-active-p)
	                         (buffer-substring-no-properties
	                          (region-beginning) (region-end))
	                       (thing-at-point 'symbol t))))
	          (consult-ripgrep default-inputp input)))
	      :config
	      (progn
	        ;; (defvar my-consult-line-map
	        ;;   (let ((map (make-sparse-keymap)))
	        ;;     (define-key map "C-s" #'previous-history-element)
	        ;;     map))
	        ;; (consult-customize consult-line :keymap my-consult-line-map)
	        ;; ;; 禁止自动显示consult文件的内容
	        (setq consult-preview-key "C-v")
	        ;; 应用 Orderless 的正则解析到 consult-grep/ripgrep/find
	        (defun consult--orderless-regexp-compiler (input type &rest _config)
	          (setq input (orderless-pattern-compiler input))
	          (cons
	           (mapcar (lambda (r) (consult--convert-regexp r type)) input)
	           (lambda (str) (orderless--highlight input str))))
	        ;; 表示的buffer种类
	        (defcustom consult-buffer-sources
	          '(consult--source-hidden-buffer
	            consult--source-buffer
	            consult--source-file
	            consult--source-bookmark
	            consult--source-project-buffer
	            consult--source-project-file)
	          "Sources used by `consult-buffer'. See `consult--multi' for a description of the source values."
	          :type '(repeat symbol))
	        ;; ？提示检索buffer类型；f<SPC>=file, p<SPC>=project, etc..
	        (define-key consult-narrow-map
	    		(vconcat consult-narrow-key "?") #'consult-narrow-help)))
	    (use-package embark
	      :after vertico
	      :bind
	      (("C-h B" . embark-bindings)  ;; alternative for `describe-bindings'
	       (:map minibuffer-local-map
	             ("C-;" . embark-act)         ;; 对函数进行设置操作 
	             ("M-." . embark-dwim)        ;; 实施 
	             ("C-c C-e" . embark-export)  ;; occur
	             )) 
	      :init
	      ;; Optionally replace the key help with a completing-read interface
	      (setq prefix-help-command #'embark-prefix-help-command)
	      :config
	      (add-to-list 'display-buffer-alist
	                   '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
	                     nil
	                     (window-parameters (mode-line-format . none)))))
	    
	    ;; embark-export弹出occur和grep mode的buffer
	    (use-package embark-consult
	      :ensure t
	      :after (consult))
	    (use-package vundo
	      :defer t
	      :commands
	      (vundo)
	      :bind
	      ("C-x u" . vundo))
	    (use-package emacs
	      :init
	      ;; 启用自动括号配对
	      (electric-pair-mode t)
	      
	      :config
	      ;; 配置 electric-pair-mode 行为
	      (setq electric-pair-preserve-balance nil)
	      ;; 使用保守的抑制策略
	      ;; https://www.reddit.com/r/emacs/comments/4xhxfw/how_to_tune_the_behavior_of_eletricpairmode/
	      (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
	      
	      ;; 保存默认的配对括号设置，以便创建模式特定的本地设置
	      (defconst idiig/default-electric-pairs electric-pair-pairs)
	      
	      ;; 为特定模式添加本地电子配对
	      (defun idiig/add-local-electric-pairs (pairs)
	        "为当前缓冲区添加本地电子配对括号。
	         
	         参数:
	           PAIRS: 要添加的括号对列表
	         
	         示例用法:
	           (add-hook 'jupyter-org-interaction-mode-hook
	                     (lambda () (idiig/add-local-electric-pairs '((?$ . ?$)))))"
	        (setq-local electric-pair-pairs (append idiig/default-electric-pairs pairs))
	        (setq-local electric-pair-text-pairs electric-pair-pairs))
	      
	      ;; 禁止自动配对尖括号 <>
	      (add-function :before-until electric-pair-inhibit-predicate
	                    (lambda (c) (eq c ?<)))
	      
	      ;; 增强的括号匹配高亮——即使光标在括号内也能高亮匹配的括号
	      (define-advice show-paren-function (:around (fn) fix-show-paren-function)
	        "即使光标不直接位于括号上，也能高亮匹配的括号。"
	        (cond ((looking-at-p "\\s(") (funcall fn))
	              (t (save-excursion
	                   (ignore-errors (backward-up-list))
	                   (funcall fn)))))
	      
	      ;; 启用括号匹配高亮
	      (show-paren-mode t))
	    (use-package puni
	      :defer t
	      :bind
	      (:map puni-mode-map
	      	([remap puni-kill-line] . idiig/puni-kill-line)
	      	([remap puni-kill-region] . idiig/puni-backward-kill-word-or-region)
	    	([remap puni-forward-delete-char] . idiig/puni-hungry-delete)
	    	([remap puni-backward-delete-char] . idiig/puni-hungry-backspace)
	      	("C-=" . puni-expand-region)
	      	("C--" . puni-contract-region))
	      :init
	      ;; The autoloads of Puni are set up so you can enable `puni-mode` or
	      ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
	      ;; any key that calls Puni commands, it's loaded.
	      (puni-global-mode)
	      (add-hook 'term-mode-hook #'puni-disable-puni-mode)
	      :config
	      (defun idiig/puni-kill-line ()
	        "Kill a line forward while keeping expressions balanced.
	      If nothing can be deleted, kill backward.  If still nothing can be
	      deleted, kill the pairs around point."
	        (interactive)
	        (let ((bounds (puni-bounds-of-list-around-point)))
	          (if (eq (car bounds) (cdr bounds))
	              (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
	                (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill))
	      	(if (eq (point) (cdr bounds))
	                (puni-backward-kill-line)
	              (puni-kill-line)))))
	    
	      (defun idiig/puni-backward-kill-word-or-region (&optional arg)
	        "如无选中则杀掉前面的单词，如有选中则杀掉选中区域"
	        (interactive "p")
	        (if (region-active-p)
	      	(call-interactively #'puni-kill-region)
	          (puni-backward-kill-word arg)))
	    
	      (defun idiig/puni-hungry-backspace ()
	        "Smart backspace function."
	        (interactive)
	        (if (looking-back (rx line-start (+ blank)))
	    	(delete-region (line-beginning-position) (point))
	          (puni-backward-delete-char)))
	    
	      (defun idiig/puni-hungry-delete ()
	        "Smart delete function."
	        (interactive)
	        (if (looking-at (rx (+ blank) (not blank)))
	    	(delete-region (point) (progn 
	    				 (skip-chars-forward " \t\f\v")
	    				 (point)))
	          (puni-forward-delete-char))))
	    ;; 添加 advice
	    (with-eval-after-load 'puni
	      (defun idiig/puni-expand-region-advice (orig-fun &rest args)
	        "使用选中后的操作"
	        
	        (let* ((ev last-command-event)
	               (echo-keystrokes nil))
	          ;; 执行初始调整
	          (apply orig-fun args)
	    
	          ;; 设置 transient map
	          (let ((delta (car args))) 
	    	(set-transient-map
	    	 (let ((map (make-sparse-keymap)))
	               ;; 持续扩大
	               (define-key map (kbd "=") 'puni-expand-region)
	               ;; 缩小范围
	               (define-key map (kbd "-") 'puni-contract-region)
	    	   ;; 其他操作
	    	   ;; 检索
	               (define-key map (kbd "/") 'idiig/consult-project-region-or-symbol)
	               (define-key map (kbd "b") 'idiig/consult-buffer-region-or-symbol)
	    	   ;; 加包围
	    	   (define-key map (kbd ")") 'puni-wrap-round)
	               (define-key map (kbd "]") 'puni-wrap-square)
	    	   (define-key map (kbd "}") 'puni-wrap-curly)
	    	   (define-key map (kbd ">") 'puni-wrap-angle)
	    	   map)
	    	 nil nil
	    	 "Use %k for further adjustment"))))
	      (advice-add 'puni-expand-region :around #'idiig/puni-expand-region-advice))
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
	    (use-package ddskk
	      :defer t
	      :bind (("C-x j" . skk-mode))
	      :config
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
	    
	      ;; 片假名转换设置
	      (setq skk-search-katakana 'jisx0201-kana)
	    
	      ;; 加载额外功能
	      (require 'skk-hint)
	      :hook
	      (skk-load . (lambda ()
	                    (require 'context-skk))))
	    (use-package pyim
	      :diminish pyim-isearch-mode
	      :commands
	      (toggle-input-method)
	      :custom
	      (default-input-method "pyim")
	      (pyim-dcache-directory (concat user-emacs-directory "pyim/dcache"))
	      (pyim-default-scheme 'quanpin)
	      (pyim-page-tooltip 'popup)
	      (pyim-page-length 4))
	    
	    ;; 加载并启用基础词库
	    (use-package pyim-basedict
	      :after pyim
	      :config
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
	    ;; 确保在 orderless 加载后再加载这些配置
	    (with-eval-after-load 'orderless
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
	    (defvar idiig/writing-environment-list '("\\.org\\'"
	                                             "\\.md\\'"
	                                             "\\.qmd\\'"
	                                             "\\.rmd\\'"
	                                             "\\.typ\\'"
	                                             "\\.tex\\'"
	                                             "\\.bib\\'"
	                                             "\\.txt\\'"))
	    (defun idiig/in-writing-environment-p ()
	      "Check if current buffer file matches any pattern in idiig/writing-environment-list."
	      (when (buffer-file-name)
	        (cl-some (lambda (pattern)
	                   (string-match-p pattern (buffer-file-name)))
	                 idiig/writing-environment-list)))
	    
	    (add-hook 'find-file-hook
	              (lambda ()
	                (when (idiig/in-writing-environment-p)
	                  (visual-line-mode 1))))
	    
	    (with-eval-after-load 'diminish
	      (diminish 'visual-line-mode))
	    (with-eval-after-load 'puni
	      (defun idiig/backward-kill-word-or-region (&optional arg)
	        (interactive "p")
	        (if (region-active-p)
	    	(call-interactively #'puni-kill-active-region)
	          (backward-kill-word arg)))
	    
	      (global-set-key (kbd "C-w") 'idiig/backward-kill-word-or-region))
	    (defun idiig/indent-buffer()
	      (interactive)
	      (indent-region (point-min) (point-max)))
	    
	    (defun idiig/indent-region-or-buffer()
	      (interactive)
	      (save-excursion
	        (if (region-active-p)
	            (progn
	              (indent-region (region-beginning) (region-end)))
	          (progn
	            (idiig/indent-buffer)))))
	    
	    (global-set-key (kbd "C-M-\\") 'idiig/indent-region-or-buffer)
	    (global-set-key (kbd "C-M-¥") 'idiig/indent-region-or-buffer)  ;; JIS keyboard
	    (global-set-key [(shift return)] 'idiig/smart-open-line)
	    (defun idiig/goto-match-paren (arg)
	      "Go to the matching if on (){}[], similar to vi style of % "
	      (interactive "p")
	      ;; first, check for "outside of bracket" positions expected by forward-sexp, etc
	      (cond ((looking-at "[\[\(\{]") (evil-jump-item))
	            ((looking-back "[\]\)\}]" 1) (evil-jump-item))
	            ;; now, try to succeed from inside of a bracket
	            ((looking-at "[\]\)\}]") (forward-char) (evil-jump-item))
	            ((looking-back "[\[\(\{]" 1) (backward-char) (evil-jump-item))
	            (t nil)))
	    
	    (bind-key* "M--" 'idiig/goto-match-paren)
	    (defun idiig/insert-space-after-point ()
	      (interactive)
	      (save-excursion (insert " ")))
	    
	    (bind-key* "C-." 'idiig/insert-space-after-point)
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
	    (defmacro idiig//setup-nix-lsp-bridge-server (language server-name executable-path &optional lib-path)
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
	      :diminish lsp-bridge-mode
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
	      :diminish yas-minor-mode
	      :hook
	      (prog-mode . yas-minor-mode)
	      :init
	      ;; (setq yas-snippet-dirs <path/to/snippets>)
	      ;; (push idiig/snippet-dir yas-snippet-dirs)
	      :config
	      (yas-reload-all))
	    (use-package consult
	      :after
	      (consult
	       yas-minor-mode))
	    
	    (idiig//setup-nix-lsp-bridge-server 
	     "nix" 
	     "nixd" 
	     "${pkgs.nixd}/bin" 
	     nil)
	    (idiig//setup-nix-lsp-bridge-server 
	     "python" 
	     "basedpyright" 
	     "${pkgs.basedpyright}/bin" 
	     "${pkgs.stdenv.cc.cc.lib}/lib")
	    (idiig//setup-nix-lsp-bridge-server 
	     "tex" 
	     "texlab" 
	     "${pkgs.texlab}/bin" 
	     nil)
	    
	    (add-to-list 'exec-path "${pkgs.texliveFull}/bin")
	    (add-hook 'org-mode-hook 'idiig/run-prog-mode-hooks)
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
	    (with-eval-after-load 'org
	      (setq org-display-remote-inline-images t))
	    (add-hook 'org-mode-hook
	              (lambda ()
	                (when (string-match-p "\\.ai\\.org\\'" (buffer-file-name))
	                  (gptel-mode 1))))
	    (add-to-list 'exec-path "${pkgs.aider-chat}/bin")
	    (use-package meow
	      :bind
	      ("C-M-s" . meow-visit)
	      :init
	      ;; https://github.com/meow-edit/meow/blob/master/KEYBINDING_QWERTY.org
	      (require 'meow)
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
	      (meow-setup)
	      :config
	      (meow-global-mode 1))
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
            mwim
            unfill
            vertico
              orderless
              marginalia
              embark
              consult
              embark-consult
            vundo
            puni
            ddskk
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
              consult-yasnippet
            
            nix-mode
            auctex
              auctex-latexmk
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
