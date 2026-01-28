{
  description = "idiig's Emacs Configuration with Nix Flakes";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    emacs-overlay.url = "github:nix-community/emacs-overlay";
    
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
	    ;; Useful decorations nil
	    (tool-bar-mode -1)			; 关闭工具栏
	    (menu-bar-mode -1)			; 关闭工具栏
	    (scroll-bar-mode -1)			; 关闭文件滑动控件
	    (setq ring-bell-function 'ignore)
	    (setq use-short-answers t)
	    (setq inhibit-splash-screen 1)     ; 关闭启动帮助画面
	    (setq initial-scratch-message nil) ; 关闭scratch message
	    (setq inhibit-startup-message t)   ; 关闭启动信息
	    (use-package so-long
	      :init
	      (global-so-long-mode +1))
	    (use-package delsel
	      :custom
	      (delete-selection-mode t))
	    (use-package autorevert
	      :custom
	      (global-auto-revert-mode t))
	    ;; frame.el
	    (use-package frame
	      :init
	      (setq initial-frame-alist '((fullscreen . maximized)))	; 全屏
	      (setq frame-title-format ; 窗口显示文件路径/buffer名
	          '("" " idiig - "
	            (:eval (if (buffer-file-name)
	                       (abbreviate-file-name (buffer-file-name)) "%b")))))
	    
	    (use-package window
	      :bind
	      (([remap enlarge-window] . idiig/smart-adjust-window-size-up)
	       ([remap shrink-window] . idiig/smart-adjust-window-size-down)
	       ([remap shrink-window-horizontally] . idiig/smart-adjust-window-size-left)
	       ([remap enlarge-window-horizontally] . idiig/smart-adjust-window-size-right))
	      
	      :init
	      (defun idiig/smart-adjust-window-size (direction &optional delta horizontal)
	        "Intelligently adjust window size based on window position.
	    DIRECTION: 'up, 'down, 'left, 'right indicates border movement direction
	    DELTA: adjustment size, default is 5
	    HORIZONTAL: whether to adjust horizontally"
	        (let ((delta (or delta 5)))
	          (pcase direction
	    	;; Vertical adjustment
	    	('up
	    	 (cond
	              ;; If there's a window below, shrink current window (border moves up)
	              ((window-in-direction 'below)
	               (shrink-window delta nil))
	              ;; If there's a window above, enlarge current window (border moves up)
	              ((window-in-direction 'above)
	               (enlarge-window delta nil))
	              (t (message "Cannot adjust window upward"))))
	    	
	    	('down
	    	 (cond
	              ;; If there's a window below, enlarge current window (border moves down)
	              ((window-in-direction 'below)
	               (enlarge-window delta nil))
	              ;; If there's a window above, shrink current window (border moves down)
	              ((window-in-direction 'above)
	               (shrink-window delta nil))
	              (t (message "Cannot adjust window downward"))))
	    	
	    	;; Horizontal adjustment
	    	('left
	    	 (cond
	              ;; If there's a window on the right, shrink current window (border moves left)
	              ((window-in-direction 'right)
	               (shrink-window delta t))
	              ;; If there's a window on the left, enlarge current window (border moves left)
	              ((window-in-direction 'left)
	               (enlarge-window delta t))
	              (t (message "Cannot adjust window leftward"))))
	    	
	    	('right
	    	 (cond
	              ;; If there's a window on the right, enlarge current window (border moves right)
	              ((window-in-direction 'right)
	               (enlarge-window delta t))
	              ;; If there's a window on the left, shrink current window (border moves right)
	              ((window-in-direction 'left)
	               (shrink-window delta t))
	              (t (message "Cannot adjust window rightward")))))))
	      
	      ;; Define convenience commands
	      (defun idiig/smart-adjust-window-size-up (&optional delta)
	        "Move border upward"
	        (interactive "P")
	        (idiig/smart-adjust-window-size 'up (or delta 5)))
	    
	      (defun idiig/smart-adjust-window-size-down (&optional delta)
	        "Move border downward"
	        (interactive "P")
	        (idiig/smart-adjust-window-size 'down (or delta 5)))
	    
	      (defun idiig/smart-adjust-window-size-left (&optional delta)
	        "Move border leftward"
	        (interactive "P")
	        (idiig/smart-adjust-window-size 'left (or delta 5)))
	    
	      (defun idiig/smart-adjust-window-size-right (&optional delta)
	        "Move border rightward"
	        (interactive "P")
	        (idiig/smart-adjust-window-size 'right (or delta 5))))
	    (use-package window
	      :init
	      ;; Add transient map support
	      (defun idiig/smart-adjust-window-size-advice (orig-fun direction &rest args)
	        "Add continuous adjustment support for idiig/smart-adjust-window-size"
	        (let ((delta (or (car args) 5)))
	          ;; Execute initial adjustment
	          (apply orig-fun direction args)
	          
	          ;; Set transient map
	          (set-transient-map
	           (let ((map (make-sparse-keymap)))
	    	 ;; ^ = border moves up
	    	 (define-key map (kbd "^")
	    		     `(lambda () (interactive) (idiig/smart-adjust-window-size 'up ,delta)))
	    	 ;; V = border moves down
	    	 (define-key map (kbd "V")
	    		     `(lambda () (interactive) (idiig/smart-adjust-window-size 'down ,delta)))
	    	 ;; { = border moves left
	    	 (define-key map (kbd "{")
	    		     `(lambda () (interactive) (idiig/smart-adjust-window-size 'left ,delta)))
	    	 ;; } = border moves right
	    	 (define-key map (kbd "}")
	    		     `(lambda () (interactive) (idiig/smart-adjust-window-size 'right ,delta)))
	    	 ;; + = balance windows
	    	 (define-key map (kbd "+")
	    		     (lambda () (interactive) (balance-windows)))
	    	 ;; M = maximize
	    	 (define-key map (kbd "M")
	    		     (lambda () (interactive) (maximize-window)))
	    	 ;; m = minimize
	    	 (define-key map (kbd "m")
	    		     (lambda () (interactive) (minimize-window)))
	    	 map)
	           nil nil
	           "Use %k for further adjustment")))
	    
	      :config
	      ;; Apply advice
	      (advice-add 'idiig/smart-adjust-window-size :around #'idiig/smart-adjust-window-size-advice))
	    (defadvice split-window-below (after split-window-below-and-switch activate)
	      "切换到新分割的窗口"
	      (when (called-interactively-p 'any)
	        (other-window 1)))
	    
	    (defadvice split-window-right (after split-window-right-and-switch activate)
	      "切换到新分割的窗口"
	      (when (called-interactively-p 'any)
	        (other-window 1)))
	    (use-package window
	      :custom
	      ;; 缓冲区显示优先级:
	      ;; 1. 复用已显示该缓冲区的窗口(避免重复显示)
	      ;; 2. 在当前窗口显示
	      ;; 3. 可跨不同 frame 查找可复用的窗口
	      (display-buffer-base-action
	       '((display-buffer-reuse-window display-buffer-same-window)
	         (reusable-frames . t)))
	      
	      ;; 让缓冲区切换遵循智能显示规则
	      (switch-to-buffer-obey-display-actions t)
	      
	      ;; 在专用窗口中切换缓冲区时弹出新窗口，保护原窗口内容
	      (switch-to-buffer-in-dedicated-window 'pop))
	    (use-package files
	      :init
	      (setq make-backup-files nil)		; 不自动生成备份文件
	      (setq require-final-newline t)	; 文件最后添加新行
	      :config
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
	                (make-directory dir t)))))))
	    (use-package recentf
	      :defer t
	      :commands (consult-recent-file)
	      :init
	      (recentf-mode 1)
	      :custom
	      ;; Save location and limits
	      (recentf-save-file (expand-file-name "recentf" user-emacs-directory))
	      (recentf-max-saved-items 2048)    ; Maximum number of recent files to save
	      (recentf-max-menu-items 10)       ; Number of items to show in menu
	      
	      ;; Exclude files that shouldn't be tracked
	      (recentf-exclude
	       '(;; Version control
	         "COMMIT_MSG" "COMMIT_EDITMSG" "github.*txt$"
	         ;; System files
	         "/tmp/" "/sudo:"
	         ;; Tag files
	         "/TAGS$" "/GTAGS$" "/GRAGS$" "/GPATH$"
	         ;; Media files
	         "\\.mkv$" "\\.mp[34]$" "\\.avi$"
	         "\\.sub$" "\\.srt$" "\\.ass$"
	         ;; Images
	         ".*png$"
	         ;; Other
	         "bookmarks"))
	      
	      :hook
	      ;; Clean up non-existent files when exiting Emacs
	      (kill-emacs . idiig/cleanup-recentf)
	      
	      :config
	      (defun idiig/cleanup-recentf ()
	        "Remove non-existent files from recent files list."
	        (when (fboundp 'recentf-cleanup)
	          (recentf-cleanup))))
	    (use-package bookmark
	      :init
	      (setq bookmark-default-file (expand-file-name "bookmarks" user-emacs-directory)
	            bookmark-save-flag 1))
	    (use-package simple
	      :custom
	      (set-mark-command-repeat-pop t))
	    (use-package saveplace
	      :init
	      (setq save-place-file (expand-file-name "place" user-emacs-directory))
	      (save-place-mode 1))
	    (use-package savehist
	      :init
	      (setq savehist-additional-variables '(search-ring ; 扩展历史记录保存：搜索历史
	    					regexp-search-ring ; 正则搜索历史
	    					register-alist	   ; 寄存器
	    					kill-ring)	   ; 剪切板历史
	            savehist-autosave-interval 60	; 每一分钟保存一次
	            savehist-file (expand-file-name "savehist" user-emacs-directory)) ; 保存文件位置
	      (savehist-mode t))			; 启用 savehist 模式
	    (use-package dired
	      :commands (dired dired-jump)
	      :custom
	      ;; Intelligently guess target directory for operations (e.g., in split windows)
	      (dired-dwim-target t)
	      
	      ;; Always delete and copy directories recursively
	      (dired-recursive-deletes 'always)
	      (dired-recursive-copies 'always)
	      
	      ;; Move files to trash instead of permanent deletion
	      (delete-by-moving-to-trash t)
	      
	      ;; Auto-revert dired buffer when directory contents change
	      (dired-auto-revert-buffer t)
	    
	      ;; i-search should search file names only
	      (dired-isearch-filenames t)
	      
	      :hook
	      (dired-mode . (lambda ()
	    		  ;; Enable auto-revert
	                      (auto-revert-mode 1)
	    		  ;; Enable highlight current line
	                      (hl-line-mode 1)
	    		  ;; Disable GNU ls flag when BSD ls
	    		  (when (file-remote-p dired-directory)
	                        (setq-local dired-actual-switches
	                                    (if (string-match-p "gnu"
	                                                        (or (ignore-errors
	                                                              (shell-command-to-string 
	                                                               "ls --version 2>/dev/null"))
	                                                            ""))
	                                        "-AGFhlv --group-directories-first --time-style=long-iso"
	                                      "-AGFhlv")))))
	      
	      :config
	      ;; Enable 'a' key to open directories in same buffer instead of creating new ones
	      (put 'dired-find-alternate-file 'disabled nil))
	    (use-package elec-pair
	      :custom
	      ;; Disable balance checking - allow pairing even when unbalanced
	      (electric-pair-preserve-balance nil)
	      
	      ;; Use conservative inhibit strategy for smart context detection
	      (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
	      
	      :config
	      ;; Enable automatic bracket pairing
	      (electric-pair-mode 1)
	      
	      ;; Save default pairs for creating mode-specific local settings
	      (defconst idiig/default-electric-pairs electric-pair-pairs
	        "Default electric pair settings to use as base for local pairs.")
	      
	      (defun idiig/add-local-electric-pairs (pairs)
	        "Add local electric pairs for the current buffer.
	           
	    Arguments:
	      PAIRS: List of character pairs to add
	           
	    Example usage:
	      (add-hook 'jupyter-org-interaction-mode-hook
	                (lambda () (idiig/add-local-electric-pairs '((?$ . ?$)))))"
	        (setq-local electric-pair-pairs (append idiig/default-electric-pairs pairs)
	                    electric-pair-text-pairs electric-pair-pairs))
	      
	      ;; Disable auto-pairing for angle brackets <>
	      (add-function :before-until electric-pair-inhibit-predicate
	                    (lambda (c) (eq c ?<))))
	    (use-package paren
	      :config
	      ;; Enable bracket matching highlight
	      (show-paren-mode 1)
	      
	      ;; Enhanced bracket matching - highlight even when cursor is inside brackets
	      (define-advice show-paren-function (:around (fn) fix-show-paren-function)
	        "Highlight matching brackets even when cursor is not directly on a bracket.
	    When cursor is inside a bracketed expression, highlight the innermost
	    enclosing bracket pair."
	        (cond ((looking-at-p "\\s(") (funcall fn))
	              (t (save-excursion
	                   (ignore-errors (backward-up-list))
	                   (funcall fn))))))
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
	      ;; Add prompt indicator for `completing-read-multiple'
	      ;; e.g., [CRM<separator>] in the minibuffer
	      (defun crm-indicator (args)
	        "Add a visual indicator showing the separator for `completing-read-multiple'."
	        (cons (format "[CRM%s] %s"
	                      (replace-regexp-in-string
	                       "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
	                       crm-separator)
	                      (car args))
	              (cdr args)))
	      (advice-add #'completing-read-multiple :filter-args #'crm-indicator)
	    
	      :hook
	      ;; Prevent cursor from entering minibuffer prompt area
	      (minibuffer-setup . cursor-intangible-mode)
	      
	      ;; Close minibuffer when mouse leaves buffer (useful for mouse-heavy workflows)
	      ;; Source: http://trey-jackson.blogspot.com/2010/04/emacs-tip-36-abort-minibuffer-when.html
	      (mouse-leave-buffer . idiig/stop-using-minibuffer)
	    
	      :custom
	      ;; Make minibuffer prompt read-only and prevent cursor from entering it
	      (minibuffer-prompt-properties
	       '(read-only t cursor-intangible t face minibuffer-prompt))
	      
	      ;; Enable recursive minibuffers (useful for complex workflows)
	      (enable-recursive-minibuffers t)
	      
	      ;; In Emacs 28+, hide commands not relevant to current mode
	      ;; This keeps the M-x interface clean and contextual
	      (read-extended-command-predicate #'command-completion-default-include-p)
	    
	      :config
	      (defun idiig/stop-using-minibuffer ()
	        "Abort minibuffer when switching to another buffer with mouse.
	    Useful for preventing stuck minibuffer states during mouse operations."
	        (when (and (>= (recursion-depth) 1) 
	                   (active-minibuffer-window))
	          (abort-recursive-edit))))
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
	      :init
	      (defvar +orderless-dispatch-alist
	      '((?% . char-fold-to-regexp)    ; %word% - 字符折叠匹配
	        (?! . orderless-without-literal) ; !word! - 排除匹配
	        (?`. orderless-initialism)    ; `word` - 首字母匹配
	        (?= . orderless-literal)      ; =word= - 字面匹配
	        (?~ . orderless-flex)))	  ; ~word~ - 弹性匹配
	      :config
	      (setq search-default-mode t)
	      
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
	             ("C-c r" . consult-recent-file)
	             ("C-c y" . consult-yasnippet)
	             ("C-c f" . consult-find)
	             ("C-c s" . consult-line)
	             ("C-c o" . consult-file-externally)
	             ("C-c p f" . consult-ripgrep)
	             (:map minibuffer-local-map
	                   ("C-c h" . consult-history)
	                   ("C-s" . #'previous-history-element)))
	      :init
	      (add-to-list 'exec-path "${pkgs.fd}/bin")
	      (add-to-list 'exec-path "${pkgs.ripgrep}/bin")
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
	             ("C-'" . embark-act)         ;; 对函数进行设置操作 
	             ("M-." . embark-dwim)        ;; 实施 
	             ("C-c C-e" . embark-export))) ;; occur 
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
	    (require 'ctrlf)
	    (ctrlf-mode +1)
	    (with-eval-after-load 'ctrlf
	      
	      ;; 定义 advice 函数
	      (defun ctrlf-set-default-style-advice (style)
	        "Advice function to set the default search style when changing styles.
	    This ensures the selected style becomes the new default for future sessions."
	        (setq ctrlf-default-search-style style))
	      
	      ;; 添加 advice
	      (advice-add 'ctrlf-change-search-style :after #'ctrlf-set-default-style-advice))
	    (use-package wgrep
	      :commands occur-mode
	      :config
	      (setq wgrep-auto-save-buffer t)
	      (setq wgrep-enable-key "e"))
	    (use-package puni
	      :defer t
	      :bind
	      (:map puni-mode-map
	      	([remap puni-kill-line] . idiig/puni-kill-line)
	      	("C--" . idiig/puni-contract-region)
	      	("C-=" . puni-expand-region))
	      :init
	      ;; The autoloads of Puni are set up so you can enable `puni-mode` or
	      ;; `puni-global-mode` before `puni` is actually loaded. Only after you press
	      ;; any key that calls Puni commands, it's loaded.
	      (puni-global-mode)
	      (add-hook 'term-mode-hook #'puni-disable-puni-mode)
	      :config
	      (defun idiig/puni-kill-line (&optional n)
	        "Kill a line forward while keeping expressions balanced.
	      If forward kill is not possible, try backward. If still nothing
	      can be deleted, kill the balanced expression around point."
	        (interactive "p")
	        (let ((bounds (puni-bounds-of-list-around-point)))
	          (cond
	           ;; Case 1: No list bounds found, try deleting surrounding sexp
	           ((null bounds)
	            (when-let ((sexp-bounds (puni-bounds-of-sexp-around-point)))
	              (puni-delete-region (car sexp-bounds) (cdr sexp-bounds) 'kill)))
	      
	           ;; Case 2: Point is at end of bounds, try backward kill
	           ((eq (point) (cdr bounds))
	            (puni-backward-kill-line))
	      
	           ;; Case 3: Default forward kill
	           (t
	            (puni-kill-line n)))))
	      (defun idiig/puni-contract-region (&optional arg)
	        "如无选中则保持 negative-argument,如有选中则缩小范围"
	        (interactive "p")
	        (if (region-active-p)
	            (call-interactively #'puni-contract-region)
	          (negative-argument arg)))
	      
	    )
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
	    (defun idiig/backward-hungry-delete-advice (orig-fun &rest args)
	      "Advice function to provide hungry delete functionality."
	      (if (or (looking-back (rx (+ blank))) (bolp))
	          (let ((start (save-excursion (skip-chars-backward " \t\f\n\r\v") (point))))
	            (delete-region start (point)))
	        (apply orig-fun args)))
	    
	    (defun idiig/apply-backward-hungry-delete-advice ()
	      "Reapply the hungry delete advice to the current DEL key binding function."
	      (let ((current-fun (key-binding (kbd "DEL"))))
	        (advice-remove current-fun #'idiig/backward-hungry-delete-advice)    ; 移除旧的 advice
	        (advice-add current-fun :around #'idiig/backward-hungry-delete-advice))) ; 应用新的 advice
	    
	    ;; 在 emacs-startup 时应用 advice
	    (add-hook 'emacs-startup-hook #'idiig/apply-backward-hungry-delete-advice)
	    
	    ;; 如果你有其他 hook 如打开某种模式时，需要重新应用 advice，可添加对应 hook，例如：
	    ;; (add-hook 'your-major-mode-hook #'idiig/reapply-backward-hungry-delete-advice)
	    (defun idiig/forward-hungry-delete-advice (orig-fun &rest args)
	      "Advice function to provide forward hungry delete functionality."
	      (if (looking-at (rx (or (1+ blank) "\n")))
	          (let ((end (save-excursion
	                       (skip-chars-forward " \t\f\v\n\r")
	                       (point))))
	            (delete-region (point) end))
	        (apply orig-fun args)))
	    
	    (defun idiig/apply-forward-hungry-delete-advice ()
	      "Apply the forward hungry delete advice to the current forward delete key binding function."
	      (let ((current-fun (key-binding (kbd "C-d"))))
	        (advice-remove current-fun #'idiig/forward-hungry-delete-advice) ; 移除旧的 advice
	        (advice-add current-fun :around #'idiig/forward-hungry-delete-advice))) ; 应用新的 advice
	    
	    ;; 在 emacs-startup 时应用 advice
	    (add-hook 'emacs-startup-hook #'idiig/apply-forward-hungry-delete-advice)
	    
	    ;; 如果你有其他 hook 需要重新应用 advice，可添加对应 hook，例如：
	    ;; (add-hook 'your-major-mode-hook #'idiig/apply-forward-hungry-delete-advice)
	    (defun idiig/backward-kill-word-or-region-advice (orig-fun &rest args)
	      "Enhance the C-w function to handle region more flexibly."
	      (if (region-active-p)
	          ;; 当有选中区域时，使用传递的参数调用原始C-w功能（例如 `puni-kill-region`）
	          (apply orig-fun args)
	        ;; 当没有选中区域时，执行删除单词操作
	        (let ((backward-kill-word-fun (or (key-binding (kbd "M-<DEL>"))
	                                          (key-binding (kbd "S-<delete>"))
	                                          'backward-kill-word))) ; 默认删除单词函数
	          (if (fboundp backward-kill-word-fun)
	              (call-interactively backward-kill-word-fun) ; 交互式调用删除单词
	            (message "No word kill bound function found for M-<DEL> or S-<delete>")))))
	    
	    (defun idiig/apply-backward-kill-word-or-region-advice ()
	      "Advice C-w to optionally kill region or word."
	      ;; 通过 `key-binding` 得到当前与 C-w 绑定的函数
	      (let ((current-fun (key-binding (kbd "C-w"))))
	        (advice-remove current-fun #'idiig/backward-kill-word-or-region-advice)
	        (advice-add current-fun :around #'idiig/backward-kill-word-or-region-advice)))
	    
	    ;; 在 emacs 启动时应用这个 advice
	    (add-hook 'emacs-startup-hook #'idiig/apply-backward-kill-word-or-region-advice)
	    (defvar idiig/fixed-width-font "Sarasa Mono J"
	      "The font to use for monospaced (fixed width) text.")
	    
	    (defvar idiig/variable-width-font "Sarasa Gothic J"
	      "The font to use for variable-pitch (document) text.")
	    
	    (defun idiig/scale-to-nearest-ten (value scale)
	      "Scale VALUE by SCALE and round to nearest multiple of 10.
	    For example: (idiig/scale-to-nearest-ten 1080 0.15) => 160"
	      (* 10 (round (/ (* value scale) 10.0))))
	    
	    (defun idiig/set-fonts-based-on-screen-size (scale)
	      "Set fonts based on the screen size.
	    SCALE is the ratio of font height to screen height (e.g., 0.15)."
	      (when (display-graphic-p)
	        (let* ((screen-height (display-pixel-height))
	    	   (my-font-height (idiig/scale-to-nearest-ten screen-height scale)))
	          my-font-height)))
	    
	    (defvar idiig/font-height
	      (idiig/set-fonts-based-on-screen-size 0.15)
	      "Font height based on screen size.")
	    
	    (defvar idiig/lower-font-height 
	      (idiig/set-fonts-based-on-screen-size 0.10)
	      "Lower font height based on screen size.")
	    
	    (add-hook 'after-init-hook
	    	  (lambda ()
	    	    (when (display-graphic-p)
	    	      
	    	      ;; 工具栏，菜单保持默认字体
	    	      (set-face-attribute 'menu nil
	    				  :inherit 'unspecified)
	    	      (set-face-attribute 'tool-bar nil
	    				  :inherit 'unspecified)
	    
	    	      ;; 设置 mode-line 字体
	    	      (set-face-attribute 'mode-line nil
	    				  :inherit 'unspecified
	    				  :height idiig/lower-font-height)
	    	      (set-face-attribute 'mode-line-inactive nil
	    				  :inherit 'unspecified
	    				  :height idiig/lower-font-height)
	    	      
	    	      (set-face-attribute 'default nil
	    				  :font idiig/fixed-width-font
	    				  :background "#fbf7f0"
	    				  :weight 'regular
	    				  :height idiig/font-height)
	    	      (set-face-attribute 'fixed-pitch nil
	    				  :font idiig/fixed-width-font
	    				  :weight 'regular
	    				  :height idiig/font-height)
	    	      (set-face-attribute 'variable-pitch nil
	    				  :font idiig/variable-width-font
	    				  :weight 'regular
	    				  :height idiig/font-height)
	    	      ;; 设置 comint 提示符字体
	    	      (set-face-attribute 'comint-highlight-prompt nil
	    				  :inherit 'fixed-pitch)
	    	      ;; 设置 minibuffer prompt 字体
	    	      (set-face-attribute 'minibuffer-prompt nil
	    				  :inherit 'fixed-pitch)
	    	      (set-face-attribute 'header-line nil
	    				  :inherit 'fixed-pitch))))
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
	    (require 'migemo)
	    ;; cmigemo(default)
	    (setq migemo-command "${pkgs.cmigemo}/bin/cmigemo")
	    (setq migemo-options '("-q" "--emacs"))
	    
	    ;; Set your installed path
	    (setq migemo-dictionary "${pkgs.cmigemo}/share/migemo/utf-8/migemo-dict")
	    
	    (setq migemo-user-dictionary nil)
	    (setq migemo-regex-dictionary nil)
	    (when (and migemo-command migemo-dictionary)
	      (migemo-init)
	      (message "Migemo initialized with dictionary: %s" migemo-dictionary))
	    (with-eval-after-load 'migemo
	      (with-eval-after-load 'ctrlf
	        (add-to-list 'ctrlf-style-alist '(migemo-regexp . (:prompt "migemo-regexp"
	    							       :translator migemo-search-pattern-get
	    							       :case-fold ctrlf-no-uppercase-regexp-p)))))
	    
	    (with-eval-after-load 'orderless
	      (defun orderless-migemo (component)
	      (let ((pattern (migemo-get-pattern component)))
	        (condition-case nil
	            (progn (string-match-p pattern "") pattern)
	          (invalid-regexp nil))))
	      
	      (add-to-list '+orderless-dispatch-alist '(?# . orderless-migemo)))
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
	    (with-eval-after-load 'ctrlf
	      
	      (defvar pyim-ctrlf-initialized nil
	        "Flag to track if pyim data has been initialized for ctrlf.")
	      
	      (defvar pyim-ctrlf-cache (make-hash-table :test 'equal)
	        "Cache for pyim-cregexp-build results.")
	      
	      (defconst pyim-ctrlf-vowels-with-mapping '("a" "e" "o")
	        "Vowels that have direct Chinese character mappings.")
	      
	      (defconst pyim-ctrlf-double-consonants '("zh" "ch" "sh")
	        "Double-letter consonants that should use regex-quote for exact matching.")
	      
	      (defun pyim-cregexp-build-lazy (str)
	        "Lazy wrapper for pyim-cregexp-build with caching."
	        (unless pyim-ctrlf-initialized
	          (message "Initializing pyim data for ctrlf...")
	          ;; 预缓存常用字符的结果
	          (call-interactively #'pyim-activate)
	          (call-interactively #'pyim-deactivate)
	          
	          (dolist (vowel pyim-ctrlf-vowels-with-mapping)
	            (let ((result (pyim-cregexp-build vowel)))
	              (puthash vowel result pyim-ctrlf-cache)))
	          (setq pyim-ctrlf-initialized t)
	          (message "Pyim data initialized."))
	        
	        ;; 判断是否使用 regex-quote
	        (if (or (and (= (length str) 1)
	                     (not (member str pyim-ctrlf-vowels-with-mapping)))
	                (member str pyim-ctrlf-double-consonants))
	            (regexp-quote str)
	          ;; 使用缓存或计算新结果
	          (or (gethash str pyim-ctrlf-cache)
	              (let ((result (pyim-cregexp-build str)))
	                (puthash str result pyim-ctrlf-cache)
	                result))))
	    
	      (add-to-list 'ctrlf-style-alist
	                   '(pinyin-regexp . (:prompt "pinyin-regexp"
	    					  :translator pyim-cregexp-build-lazy
	    					  :case-fold ctrlf-no-uppercase-regexp-p
	    					  :fallback (isearch-forward-regexp
	    						     . isearch-backward-regexp)))))
	    ;; (with-eval-after-load 'orderless
	    ;;   ;; 拼音检索字符串功能
	    ;;   (defun zh-orderless-regexp (orig_func component)
	    ;;     (call-interactively #'pyim-activate)
	    ;;     (call-interactively #'pyim-deactivate)
	    ;;     (let ((result (funcall orig_func component)))
	    ;;       (pyim-cregexp-build result)))
	    ;;   (advice-add 'orderless-regexp :around #'zh-orderless-regexp))
	    
	    (with-eval-after-load 'orderless
	    
	      (defvar pyim-orderless-initialized nil
	        "Flag to track if pyim data has been initialized for orderless.")
	    
	      (defun orderless-pyim (component)
	        (unless pyim-orderless-initialized
	          (message "Initializing pyim for orderless...")
	          ;; 预缓存常用字符的结果
	          (call-interactively #'pyim-activate)
	          (call-interactively #'pyim-deactivate)
	          (setq pyim-orderless-initialized t)
	          (message "Pyim data initialized."))
	        
	        (let ((pattern (pyim-cregexp-build component)))
	          (condition-case nil
	              (progn (string-match-p pattern "") pattern)
	    	(invalid-regexp nil))))
	    
	      (add-to-list '+orderless-dispatch-alist '(?@ . orderless-pyim)))
	    (use-package magit
	      :bind ("C-x g" . magit-status)
	      :commands magit-status
	      :init
	      ;; 使用nix路径中的git
	      (add-to-list 'exec-path "${pkgs.git}/bin"))
	    (with-eval-after-load 'tramp
	      (setq tramp-default-remote-shell 
	            (or (executable-find "zsh")
	                (executable-find "nushell")
	                (executable-find "bash"))))
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
	      "跳轉到匹配的括號，類似 vi 的 %。"
	      (interactive "p")
	      (cond
	       ;; 光標在開括號上
	       ((looking-at "[[({]") 
	        (forward-sexp))
	       ;; 光標在閉括號上
	       ((looking-back "[])}]" 1) 
	        (backward-sexp))
	       ;; 光標在閉括號前（例如中間空格）
	       ((looking-at "[])}]") 
	        (forward-char)
	        (backward-sexp))
	       ;; 光標在開括號後
	       ((looking-back "[[({]" 1)
	        (backward-char)
	        (forward-sexp))
	       ;; 其他情況
	       (t
	        (message "未找到匹配的括號"))))
	    
	    (bind-key* "M--" 'idiig/goto-match-paren)
	    (defun idiig/insert-space-after-point ()
	      (interactive)
	      (save-excursion (insert " ")))
	    
	    (bind-key* "C-." 'idiig/insert-space-after-point)
	    (use-package spacious-padding
	      :defer t
	      :config
	      (setq spacious-padding-widths
	            '( :internal-border-width 15
	               :header-line-width 4
	               :mode-line-width 6
	               :tab-width 4
	               :right-divider-width 30
	               :scroll-bar-width 8))
	    
	      ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
	      ;; is very flexible and provides several examples.
	      (setq spacious-padding-subtle-mode-line
	            `( :mode-line-active 'default
	               :mode-line-inactive vertical-border)))
	    ;; TODO: 这里未来需要改成在每个语言的设定的节点push进来
	    (defvar idiig/language-list
	      '("emacs-lisp" "python" "ditaa" "plantuml" "shell" "nix"
	        "R" "haskell" "latex" "css" "lisp" "jq" "makefile" "go")
	      "支持的编程语言列表。")
	    
	    (defun idiig/run-prog-mode-hooks ()
	      "Runs `prog-mode-hook'. 针对一些本该为编程语言又没自动加载prog mode的语言hook.
	    如：(add-hook 'python-hook 'idiig/run-prog-mode-hooks)
	    "
	      (run-hooks 'prog-mode-hook))
	    (defvar idiig/lsp-extra-paths nil
	      "Emacs 侧已配置的 LSP 可执行目录清单。会被写入到项目的 .emacs-lsp-paths。")
	    
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
	            `(progn
	    	   (add-to-list 'exec-path ,executable-path)
	    	   (add-to-list 'idiig/lsp-extra-paths ,executable-path)))
	         
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
	    (with-eval-after-load 'lsp-bridge
	      (defun idiig/acm-prefer-lsp-all ()
	        (when (bound-and-true-p lsp-bridge-mode)
	          ;; 让 search 后端慢一点再来（避免覆盖 LSP）
	          (when (boundp 'acm-backend-search-delay)
	            (setq-local acm-backend-search-delay 0.8))  ;; 你可调成 0.6~1.0
	    
	          ;; 提高 LSP 优先级，降低 search 优先级（若有这些变量）
	          (when (boundp 'acm-backend-lsp-priority)
	            (setq-local acm-backend-lsp-priority 100))
	          (when (boundp 'acm-backend-search-priority)
	            (setq-local acm-backend-search-priority 0))
	    
	          ;; 可选：减少噪声（若存在这些开关）
	          (when (boundp 'acm-enable-dabbrev)
	            (setq-local acm-enable-dabbrev nil))          ; 关闭 dabbrev 后端
	          (when (boundp 'acm-backend-search-candidates-min-length)
	            (setq-local acm-backend-search-candidates-min-length 3)))) ; 至少 3 字符再搜
	    
	      (add-hook 'lsp-bridge-mode-hook #'idiig/acm-prefer-lsp-all))
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
	    (use-package consult-yasnippet
	      :after
	      (consult
	       yas-minor-mode))
	    (use-package citre
	      :commands (citre-jump
	    	     citre-jump-back
	    	     citre-ace-peek
	    	     citre-update-this-tags-file)
	      :custom
	      (citre-readtags-program "${pkgs.universal-ctags}/bin/readtags")
	      (citre-ctags-program "${pkgs.universal-ctags}/bin/ctags")
	      (citre-project-root-function #'vc-dir-root)
	      ;; (citre-default-create-tags-file-location 'global-cache)
	      (citre-edit-ctags-options-manually nil)
	      (citre-auto-enable-citre-mode-modes '(prog-mode))
	      :init
	      (require 'citre-config))
	    
	    (use-package direnv
	      :defer t
	      :init
	      (add-to-list 'exec-path "${pkgs.direnv}/bin")
	      :config
	      (direnv-mode))
	    (require 'cl-lib)
	    
	    (defun idiig/project-root ()
	      "返回当前 buffer 对应的项目根（优先含 .envrc，其次 .git）。"
	      (or (locate-dominating-file default-directory ".envrc")
	          (locate-dominating-file default-directory ".git")
	          default-directory))
	    
	    (defun idiig/write-emacs-lsp-paths ()
	      "将 `idiig/lsp-extra-paths` 写入项目根的 .emacs-lsp-paths。"
	      (interactive)
	      (when-let* ((root (idiig/project-root))
	                  (file (expand-file-name ".emacs-lsp-paths" root)))
	        (let* ((dirs (->> idiig/lsp-extra-paths
	                          (seq-filter #'file-directory-p)
	                          (delete-dups))))
	          (when dirs
	            (with-temp-file file
	              (dolist (p dirs) (insert p "\n")))))))
	    
	    ;; lsp-bridge 项目根识别（避免偶发 no views）
	    ;; direnv 集成：allow/refresh 前写清单；完成后自动重启 lsp-bridge
	    (with-eval-after-load 'direnv
	      ;; before：生成/更新 .emacs-lsp-paths，供 .envrc 读取
	      (advice-add 'direnv-allow :before (lambda (&rest _) (idiig/write-emacs-lsp-paths)))
	      (when (fboundp 'direnv-update-environment)
	        (advice-add 'direnv-update-environment :before
	                    (lambda (&rest _) (idiig/write-emacs-lsp-paths))))
	    
	      ;; after：环境就绪后，如有需要自动重启 lsp-bridge
	      (defun idiig/direnv--restart-lsp-bridge (&rest _)
	        (when (and (featurep 'lsp-bridge)
	                   (fboundp 'lsp-bridge-restart-process)
	                   (cl-some (lambda (buf)
	                              (with-current-buffer buf
	                                (bound-and-true-p lsp-bridge-mode)))
	                            (buffer-list)))
	          (lsp-bridge-restart-process)))
	      (advice-add 'direnv-allow :after #'idiig/direnv--restart-lsp-bridge)
	      (when (fboundp 'direnv-update-environment)
	        (advice-add 'direnv-update-environment :after #'idiig/direnv--restart-lsp-bridge)))
	    (add-hook 'eshell-load-hook #'eat-eshell-mode)
	    (add-hook 'eshell-load-hook #'eat-eshell-visual-command-mode)
	    (idiig//setup-nix-lsp-bridge-server 
	     "nix" 
	     "nixd" 
	     "${pkgs.nixd}/bin" 
	     nil)
	    (use-package slime
	      :init
	      (setq inferior-lisp-program
	    	(or (executable-find "sbcl")
	    	    "${pkgs.sbcl}/bin/sbcl"))
	      :config
	      (slime-setup '(slime-fancy)))
	    (add-hook 'eval-expression-minibuffer-setup 'idiig/run-prog-mode-hooks)
	    (idiig//setup-nix-lsp-bridge-server 
	     "clojure" 				; language name
	     "clojure-lsp" 				; lsp name
	     "${pkgs.clojure-lsp}/bin"		; dependency nixpkg path
	     nil)					; other dependencies
	    (idiig//setup-nix-lsp-bridge-server 
	     "python" 
	     "basedpyright" 
	     "${pkgs.basedpyright}/bin" 
	     "${pkgs.stdenv.cc.cc.lib}/lib")
	    (idiig//setup-nix-lsp-bridge-server 
	     "haskell"
	     "hls" 
	     "${pkgs.haskell-language-server}/bin" 
	     nil)
	    (idiig//setup-nix-lsp-bridge-server 
	     "go" 
	     "gopls" 
	     "${pkgs.gopls}/bin" 
	     nil)
	    (defun idiig/go-prefer-lsp ()
	      (when (derived-mode-p 'go-mode 'go-ts-mode)
	        ;; 关闭文件内/跨缓冲词搜索后端（如果你的版本有这些开关）
	        (when (boundp 'acm-enable-search-file-words)
	          (setq-local acm-enable-search-file-words nil))
	        (when (boundp 'acm-enable-dabbrev)
	          (setq-local acm-enable-dabbrev nil))
	        ;; 把搜索词的延迟调大，避免覆盖（若有这个变量）
	        (when (boundp 'acm-backend-search-delay)
	          (setq-local acm-backend-search-delay 0.8))
	        ;; LSP 候选最短前缀更短一些（若有）
	        (when (boundp 'acm-backend-lsp-candidate-min-length)
	          (setq-local acm-backend-lsp-candidate-min-length 0))))
	    (add-hook 'go-mode-hook #'idiig/go-prefer-lsp)
	    (add-hook 'go-ts-mode-hook #'idiig/go-prefer-lsp)
	    (idiig//setup-nix-lsp-bridge-server 
	     "bash" 				; language name
	     "bash-language-server" 		; lsp name
	     "${pkgs.bash-language-server}/bin" 	; dependency nixpkg path
	     nil)					; other dependencies
	    (idiig//setup-nix-lsp-bridge-server 
	     "tex" 
	     "texlab" 
	     "${pkgs.texlab}/bin" 
	     nil)
	    (add-hook 'TeX-mode-hook 'idiig/run-prog-mode-hooks)
	    (use-package auctex
	      :defer t)
	    (idiig//setup-nix-lsp-bridge-server 
	     "typst" 
	     "tinymist" 
	     "${pkgs.tinymist}/bin" 
	     nil)
	    (idiig//setup-nix-lsp-bridge-server 
	     "json" 
	     "vscode-json-language-server" 
	     "${pkgs.vscode-langservers-extracted}/bin" 
	     nil)
	    (use-package jsonian
	      :after so-long
	      :mode ("\\.json\\'" . jsonian-mode)
	      :init
	      (add-to-list 'major-mode-remap-alist '(json-mode . jsonian-mode))
	      (add-to-list 'major-mode-remap-alist '(json-ts-mode . jsonian-mode))
	      :custom
	      (jsonian-no-so-long-mode t))
	    (add-to-list 'exec-path "${pkgs.plantuml}/bin")
	    (with-eval-after-load 'org
	      (setq org-plantuml-jar-path "${pkgs.plantuml}/lib/plantuml.jar")
	      (setq org-plantuml-executable-path "${pkgs.plantuml}/bin/plantuml")
	      (setq org-plantuml-exec-mode 'plantuml))
	    (add-to-list 'exec-path "${pkgs.graphviz}/bin")
	    (add-hook 'org-mode-hook 'idiig/run-prog-mode-hooks)
	    (defun idiig/load-org-babel-languages ()
	      "根据 `idiig/language-list` 启用 `org-babel` 语言。"
	      (let ((languages '()))
	        (dolist (lang idiig/language-list)
	          (push (cons (intern lang) t) languages)) ;; 将字符串转换为符号
	        (org-babel-do-load-languages 'org-babel-load-languages languages)))
	    
	    (defun idiig/set-org-babel-language-commands ()
	      "根据 `idiig/language-list` 甚至语言的命令。"
	      (dolist (lang idiig/language-list)
	        (let ((var-name (intern (format "org-babel-%s-command" lang))))
	          (when (boundp var-name)
	    	(set var-name (executable-find lang))))))
	    
	    (add-hook 'org-mode-hook #'idiig/load-org-babel-languages)
	    (add-hook 'org-mode-hook #'idiig/set-org-babel-language-commands)
	    
	    ;; 特殊
	    (setq org-babel-shell-command (executable-find "bash"))
	    (with-eval-after-load 'org
	      (defun idiig/org-insert-structure-template-src-advice (orig-fun type)
	        "Advice for org-insert-structure-template to handle src blocks."
	        (if (string= type "src")  ; 判断条件为 "src"
	    	(let ((selected-type (ido-completing-read "Source code type: " idiig/language-list)))
	    	  (funcall orig-fun (format "src %s" selected-type)))
	          (funcall orig-fun type)))
	    
	      (advice-add 'org-insert-structure-template :around #'idiig/org-insert-structure-template-src-advice))
	    (with-eval-after-load 'org
	      (setq org-startup-with-inline-images t) ; 启动时显示图片
	      (setq org-startup-with-latex-preview t) ; 启动时显示 LaTeX 公式
	      (add-hook 'org-mode-hook
	                (lambda ()
	                  (org-overview)		  ; 显示所有顶层节点
	    	      (org-show-entry)		  ; 显示当前节点内容
	                  (org-show-children)	  ; 显示所有子节点但不展开
	    	      (org-fold-hide-block-all)	  ; 隐藏所有代码块
	    	      (org-fold-hide-drawer-all)))) ; 隐藏所有抽屉
	    (with-eval-after-load 'org		
	      (setq org-support-shift-select 2
	    	org-catch-invisible-edits 'show-and-error ; 编辑折叠内容时显示并报错提醒
	    	org-special-ctrl-a/e t ; 增强 C-a/C-e，先跳到内容开始/结束，再跳到行首/尾
	    	org-insert-heading-respect-content t ; 插入标题时考虑内容结构，在内容后插入
	    	org-export-allow-bind-keywords t     ; 允许 =#+bind= 关键词
	    	org-display-remote-inline-images t)) ; 远程图片文件可以通过 =C-u C-c C-x C-v= 被看到
	    (defun idiig/org-show-current-only ()
	      "收起全部，只展開當前 heading 的一層。"
	      (when (org-at-heading-p)
	        (save-excursion
	          (org-overview)            ; 收起全部
	          (org-show-entry)          ; 展開當前條目內容
	          (org-show-children 1)     ; 展開當前層的子節點標題
	          (while (org-up-heading-safe) ; 保證父鏈條也可見
	            (org-show-entry)
	            (org-show-children 1)))))
	    
	    (defun idiig/org-focus-after-cycle (&rest _args)
	      "在 org-cycle 之後自動只展開當前 heading。"
	      (when (and (org-at-heading-p)
	                 (memq org-cycle-subtree-status '(children subtree)))
	        (idiig/org-show-current-only)))
	    
	    (add-hook 'org-cycle-hook #'idiig/org-focus-after-cycle)
	    (with-eval-after-load 'org
	      ;; Edit settings
	      (setq org-auto-align-tags nil		; 禁用标签自动对齐功能
	    	org-tags-column 0		; 标签紧贴标题文本，不右对齐
	    	
	    	;; Org styling, hide markup etc.
	    	org-hide-emphasis-markers nil ; 隐藏强调标记符号 (*粗体* 显示为 粗体)
	    	org-pretty-entities nil)	  ; 美化显示实体字符 (\alpha 显示为 α)
	    
	      ;; Modes
	      (auto-fill-mode 0)			; Disable auto-fill mode
	      (visual-line-mode 1)			; Enable visual-line mode for soft wrapping
	    
	      ;; faces
	      (when (display-graphic-p)
	        (set-face-attribute 'org-block nil
	      			:background "#fbf7f0"
	    			:height idiig/lower-font-height
	    			:box nil)
	        (set-face-attribute 'org-code nil
	    			:height idiig/lower-font-height)
	        (set-face-attribute 'org-tag nil
	    			:height idiig/lower-font-height)
	        (set-face-attribute 'org-table nil
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-target nil
	    			:inherit 'variable-pitch
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-block-begin-line nil
	      			:inherit '(org-block)
	    			:background "#efe9dd"
	      			:underline nil
	    			:overline "gray40"
	    			:height idiig/lower-font-height
	    			:box nil
	    			:extend nil)
	        (set-face-attribute 'org-block-end-line nil
	      			:inherit 'org-hide
	      			:height idiig/lower-font-height
	    			:overline "grey40"
	      			:extend t)
	        (with-eval-after-load 'oc
	          (set-face-attribute 'org-cite nil
	      			  :slant 'italic))))
	    (defun idiig/org-mode-face-settings ()
	      "Set custom face attributes for Org mode headings in current buffer only."
	      
	      ;; Org styling, hide markup etc.
	      (when (display-graphic-p)
	        org-hide-emphasis-markers t ; 隐藏强调标记符号 (*粗体* 显示为 粗体)
	        org-pretty-entities t	      ; 美化显示实体字符 (\alpha 显示为 α)
	        (auto-fill-mode 0)			; Disable auto-fill mode
	        (require 'org-indent)			; Ensure org-indent is loaded
	        (org-indent-mode)			; Enable org-indent mode
	        (variable-pitch-mode 1)		; Enable variable-pitch mode
	        (visual-line-mode 1)			; Enable visual-line mode for soft wrapping
	        (setq header-line-format " ")
	        
	        ;; org headings 设置行间距
	        (defface idiig-base-line
	          '((t (:inherit 'variable-pitch
	      		     :height 0.4)))
	          "Used in text-mode and friends for exactly one space after a period.")
	        
	        (let ((faces '((org-level-1 . 1.2)
	                       (org-level-2 . 1.1)
	                       (org-level-3 . 1.05)
	                       (org-level-4 . 1.0)
	                       (org-level-5 . 1.1)
	                       (org-level-6 . 1.1)
	                       (org-level-7 . 1.1)
	                       (org-level-8 . 1.1))))
	          (dolist (face faces)
	    	(face-remap-add-relative (car face) nil
	      				 :inherit 'idiig-base-line
	      				 :weight 'bold
	      				 :height (cdr face))))
	        
	        (set-face-attribute 'org-block nil
	      			:height idiig/lower-font-height
	    			:background "#efe9dd"
	    			:box nil)
	        (set-face-attribute 'org-table nil
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-formula nil
	      			:inherit 'fixed-pitch)
	        (set-face-attribute 'org-code nil
	      			:inherit '(shadow fixed-pitch)
	    			:height idiig/font-height)
	        (set-face-attribute 'org-verbatim nil
	      			:inherit '(shadow fixed-pitch))
	        (set-face-attribute 'org-special-keyword nil
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-meta-line nil
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-property-value nil
	      			:inherit '(shadow fixed-pitch)
	    			:height idiig/lower-font-height)
	        (set-face-attribute 'org-tag nil
	    			:inherit '(shadow fixed-pitch)
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-checkbox nil
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-drawer nil
	      			:height idiig/lower-font-height
	      			:extend nil)
	        (set-face-attribute 'org-block-begin-line nil
	      			:inherit '(org-block)
	      			:height idiig/font-height
	      			;; :underline "gray40"
	    			:underline nil
	    			:overline "gray40"
	    			:box nil
	    			:extend nil)
	        (set-face-attribute 'org-block-end-line nil
	      			:inherit 'org-hide
	      			:height idiig/lower-font-height 
	      			:extend )
	        (set-face-attribute 'org-link nil
	      			:inherit 'mode-line
	    			:height idiig/font-height)
	        (set-face-attribute 'org-indent nil
	      			:inherit '(org-hide fixed-pitch)
	    			:height idiig/font-height)
	        (set-face-attribute 'org-target nil
	      			:inherit '(shadow fixed-pitch)
	      			:height idiig/lower-font-height)
	        (set-face-attribute 'org-cite nil
	      			:inherit 'variable-pitch
	    			:slant 'italic)
	    
	        ;; Make the document title a bit bigger
	        (set-face-attribute 'org-document-title nil
	      			:inherit 'variable-pitch
	      			:weight 'bold
	      			:height 1.3)
	    
	        (setq-local line-spacing 0.3)
	        
	        (with-eval-after-load 'diminish
	          (diminish 'org-indent-mode)
	          (diminish 'buffer-face-mode))))
	    
	    ;; (add-hook 'org-mode-hook 'idiig/org-mode-face-settings)
	    (with-eval-after-load 'org
	      (add-to-list
	       'org-preview-latex-process-alist
	       '(idiig-dvisvgm
	         :programs ("${pkgs.texliveMedium}/bin/latex" "${pkgs.texliveMedium}/bin/dvisvgm")
	         :description "latex -> dvi -> svg (nix-store)"
	         :message "use latex and dvisvgm from nix-store."
	         :image-input-type "dvi"
	         :image-output-type "svg"
	         :image-size-adjust (0.8 . 1.0)
	         :latex-compiler ("${pkgs.texliveMedium}/bin/latex -interaction nonstopmode -output-directory %o %f")
	         :image-converter ("${pkgs.texliveMedium}/bin/dvisvgm %f --no-fonts --exact-bbox --scale=%S --output=%O")))
	    
	      (setq org-latex-create-formula-image-program 'idiig-dvisvgm))
	    (use-package org-bullets
	      :after org
	      ;; :hook (org-mode . org-bullets-mode)
	      :custom
	      (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))
	    
	    ;; (font-lock-add-keywords 'org-mode
	    ;;                         '(("^ *\\([-]\\) "
	    ;;                            (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))
	    
	    ;; (with-eval-after-load 'org
	    ;;   (setq org-ellipsis " ▾"
	    ;;         org-hide-emphasis-markers t))
	    (use-package valign
	      :diminish valign-mode
	      :hook (org-mode . valign-mode))
	    (with-eval-after-load 'org
	      (setq org-cite-export-processors
	          '((latex biblatex)
	            (html csl)
	            (odt  csl)
	            (t    biblatex))))
	    (with-eval-after-load 'oc
	      (require 'oc-basic)
	      (require 'subr-x)
	    
	      ;; 样式选择和参数输入
	      (defun my/oc--ask-style-and-affixes ()
	        "询问 citation 样式和附加参数。"
	        (let* ((style-help "
	    Style examples:
	      cite/a/cf → Citeauthor – Aa, Bb, and Cc
	      cite/a/c  → Citeauthor – Aa et al.
	      cite/na   → citeyear – 2022
	      cite/na/b → citeyearpar – (2022)
	      cite/t/c  → Citet – Aa et al. (2022)
	      cite/t/cf → citep* – Aa, Bb, and Cc (2022)
	      cite/bc   → Cite – Aa et al. 2022
	      cite/cf   → Citep – (Aa et al. 2022)")
	               (presets '(("cite/a/cf" . ("a"  . "cf"))
	                          ("cite/a/c"  . ("a"  . "c"))
	                          ("cite/na"   . ("na" . ""))
	                          ("cite/na/b" . ("na" . "b"))
	                          ("cite/t/c"  . ("t"  . "c"))
	                          ("cite/t/cf" . ("t"  . "cf"))
	                          ("cite/bc"   . ("bc" . ""))
	                          ("cite/cf"   . ("cf" . ""))))
	               (choice (completing-read
	                        "Citation style (? for help): "
	                        (append (mapcar #'car presets) '("custom" "?"))
	                        nil t nil nil "cite/cf"))
	               (l "") (b ""))
	          
	          ;; 显示帮助
	          (when (string= choice "?")
	            (message "%s" style-help)
	            (sit-for 3)
	            (setq choice (completing-read "Citation style: "
	                                          (append (mapcar #'car presets) '("custom"))
	                                          nil t nil nil "cite/cf")))
	          
	          ;; 获取样式
	          (if (not (string= choice "custom"))
	              (let ((p (cdr (assoc choice presets))))
	                (setq l (car p) b (cdr p)))
	            (setq l (completing-read "Style (l): " '("a" "na" "t" "bc" "c" "cf" "") nil t ""))
	            (setq b (completing-read "Variant (b): " '("b" "c" "cf" "") nil t "")))
	          
	          ;; 构建完整样式和参数
	          (let* ((style (string-join (seq-filter (lambda (x) (and x (not (string-empty-p x))))
	                                                 (list l b))
	                                     "/"))
	                 (prefix  (read-string "Prefix (optional): "))
	                 (locator (read-string "Locator (optional): "))
	                 (suffix  (read-string "Suffix (optional): ")))
	            (list style prefix locator suffix))))
	    
	      ;; 修改插入点的 citation
	      (defun my/oc--rewrite-cite-at-point (style prefix locator suffix)
	        "在当前位置修改 citation 的样式和参数。"
	        (save-excursion
	          (let* ((ctx (org-element-context))
	                 (cite (if (eq (org-element-type ctx) 'citation)
	                           ctx
	                         (when (re-search-backward "\\[cite\\>" (max (point-min) (- (point) 1000)) t)
	                           (org-with-point-at (match-beginning 0)
	                             (org-element-citation-parser))))))
	            (when cite
	              (let* ((beg (org-element-begin cite))
	                     (end (org-element-end   cite))
	                     (has-style (save-excursion (goto-char beg) (looking-at-p "\\[cite/")))
	                     (has-colon (save-excursion
	                                  (goto-char beg)
	                                  (re-search-forward "\\[cite\\(?:/[^:]]+\\)?\\(:\\)" end t))))
	                
	                ;; 添加样式
	                (when (and (not has-style) (not (string-empty-p style)))
	                  (goto-char (+ beg 5))
	                  (insert "/" style)
	                  (setq end (+ end (length style) 1)))
	                
	                ;; 添加前缀
	                (when (not (string-empty-p prefix))
	                  (unless has-colon
	                    (goto-char beg)
	                    (re-search-forward "\\[cite\\(?:/[^:]]+\\)?" end t)
	                    (insert ":")
	                    (setq end (1+ end)))
	                  (goto-char beg)
	                  (re-search-forward ":" end t)
	                  (insert prefix " ")
	                  (setq end (+ end (length prefix) 1)))
	                
	                ;; 添加定位符
	                (when (not (string-empty-p locator))
	                  (goto-char beg)
	                  (when (re-search-forward "@[^; \t\n]+" end t)
	                    (insert " " locator)
	                    (setq end (+ end (length locator) 1))))
	                
	                ;; 添加后缀
	                (when (not (string-empty-p suffix))
	                  (goto-char (1- end))
	                  (insert " " suffix)))))))
	    
	      ;; 主 advice：插入后弹出二次对话
	      (defun my/oc-insert-then-ask (orig-fn arg)
	        "插入引用后询问样式和参数。"
	        (let ((ret (funcall orig-fn arg)))
	          (run-at-time
	           0 nil
	           (lambda ()
	             (condition-case err
	                 (pcase-let ((`(,style ,prefix ,locator ,suffix)
	                              (my/oc--ask-style-and-affixes)))
	                   (my/oc--rewrite-cite-at-point style prefix locator suffix))
	               (quit  (message "Citation style canceled"))
	               (error (message "Error setting citation style: %s" (error-message-string err))))))
	          ret))
	    
	      ;; 挂载 advice
	      (advice-add 'org-cite-insert :around #'my/oc-insert-then-ask))
	    (use-package vulpea
	      :after org
	      :config
	      (defvar idiig/vulpea-directory
	        (concat user-emacs-directory "vulpea/")
	        "Path to the Vulpea articles directory.")
	      (defvar idiig/vulpea-db-path
	        (concat idiig/vulpea-directory "notes")
	        "Path to the Vulpea database file.")
	      (defvar idiig/vulpea-db-repo-url "git@github.com:idiig/notes.git"
	        "URL of the Vulpea database git repository.")
	      (unless (file-exists-p idiig/vulpea-db-path)
	        (make-directory idiig/vulpea-directory t)
	        (let ((result (shell-command 
	                       (format "git clone %s %s" 
	                               idiig/vulpea-db-repo-url 
	                               idiig/vulpea-db-path))))
	          (when (not (= result 0))
	            (warn "Failed to clone Vulpea database repository"))))
	      (setq vulpea-db-sync-directories `(,idiig/vulpea-db-path))
	      (vulpea-db-sync-full-scan)
	      (vulpea-db-autosync-mode +1))
	    (use-package ox-reveal
	      :after org
	      :init
	      (setq org-reveal-root "http://cdn.jsdelivr.net/reveal.js/3.0.0/")
	      (setq org-reveal-mathjax t))
	    (use-package ox-typst
	      :after org)
	    (use-package dslide
	      :commands (dslide-deck-develop dslide-deck-start)
	      :custom
	      (dslide-breadcrumb-separator " » "))
	    (defun idiig/get-sops-secret-value (key &optional path)
	      "Get secret value from SOPS-encrypted file.
	    KEY is the key to lookup in the YAML file.
	    PATH is the path to the secrets file (default: ~/.config/secrets.yaml).
	    
	    If the secrets file doesn't exist, display a helpful message with setup instructions."
	      (let ((secrets-file (expand-file-name (or path "~/.config/secrets.yaml"))))
	        (if (file-exists-p secrets-file)
	            (let ((result (string-trim
	                           (shell-command-to-string
	    			(format "${pkgs.sops}/bin/sops -d %s | ${pkgs.yq-go}/bin/yq -r '.%s'" 
	    				(shell-quote-argument secrets-file)
	    				key)))))
	              (if (or (string-empty-p result)
	                      (string= result "null"))
	                  (error "Key '%s' not found in %s" key secrets-file)
	                result))
	          (error "Secrets file not found: %s
	    
	    To set up SOPS secrets:
	    
	    1. Install required tools:
	       - sops: nix-env -iA nixpkgs.sops (or brew install sops)
	       - yq: nix-env -iA nixpkgs.yq-go (or brew install yq)
	       - age: nix-env -iA nixpkgs.age (or brew install age)
	    
	    2. Generate an age key:
	       mkdir -p ~/.config/sops/age
	       age-keygen -o ~/.config/sops/age/keys.txt
	    
	    3. Create .sops.yaml config:
	       cat > ~/.config/.sops.yaml << EOF
	       creation_rules:
	         - path_regex: secrets\\.yaml$
	           age: $(grep 'public key:' ~/.config/sops/age/keys.txt | cut -d: -f2 | tr -d ' ')
	       EOF
	    
	    4. Create and edit your secrets file:
	       sops %s
	    
	    5. Add your secrets in YAML format:
	       gh_pat_mcp: your_github_token_here
	       openai_key: your_openai_key_here
	    
	    For more info: https://github.com/getsops/sops" secrets-file))))
	    (use-package copilot
	      :config
	      (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
	      (add-to-list 'copilot-indentation-alist '(prog-mode 2))
	      (add-to-list 'copilot-indentation-alist '(org-mode 2))
	      (add-to-list 'copilot-indentation-alist '(text-mode 2))
	      (add-to-list 'copilot-indentation-alist '(lisp-mode 2))
	      (add-to-list 'copilot-indentation-alist '(emacs-lisp-mode 2))
	    
	      (setq copilot-max-char 99999999))
	    (use-package mcp
	      :after gptel
	      :custom
	      (mcp-hub-servers
	       `(("github" . (:command "${pkgs.nodejs}/bin/npx"
	    			   :args ("-y" "@modelcontextprotocol/server-github")
	    			   :env (:GITHUB_PERSONAL_ACCESS_TOKEN ,(idiig/get-sops-secret-value "gh_pat_mcp"))))
	         ("duckduckgo" . (:command "${pkgs.uv}/bin/uvx"
	    			       :args ("--python" "${pkgs.python313}/bin/python3.13"
	    				      "duckduckgo-mcp-server")))
	         ("nixos" . (:command "${pkgs.uv}/bin/uvx"
	    			  :args ("--python" "${pkgs.python313}/bin/python3.13"
	    				 "mcp-nixos")))
	         ("fetch" . (:command "${pkgs.uv}/bin/uvx"
	    			  :args ("--python" "${pkgs.python313}/bin/python3.13"
	    				 "mcp-server-fetch")))
	         ("filesystem" . (:command "${pkgs.nodejs}/bin/npx"
	    			       :args ("-y" "@modelcontextprotocol/server-filesystem"
	    				      ,(getenv "HOME"))))
	         ("sequential-thinking" . (:command "${pkgs.nodejs}/bin/npx"
	    					:args ("-y" "@modelcontextprotocol/server-sequential-thinking")))
	         ("context7" . (:command "${pkgs.nodejs}/bin/npx"
	    			     :args ("-y" "@upstash/context7-mcp")
	    			     :env (:DEFAULT_MINIMUM_TOKENS "6000")))))
	      :config (require 'mcp-hub)
	      :hook
	      (after-init . (lambda () (run-with-idle-timer 1 nil #'mcp-hub-get-all-tool)))
	      (gptel-mode . gptel-mcp-connect))
	    (use-package gptel
	      :commands (gptel-mode
	    	     gptel-chat
	    	     gptel-complete
	    	     gptel-menu
	    	     gptel-fn-complete
	    	     gptel-magit-install)
	      :hook (org-mode . (lambda ()
	                          (when (and buffer-file-name
	                                     (string-match-p "\\.ai\\.org\\'" buffer-file-name))
	                            (gptel-mode 1))))
	      :init
	      (defvar idiig/copilot-model-list
	        '(gpt-5-codex
	          claude-sonnet-4.5
	          claude-sonnet-4
	          claude-opus-4.1
	          gpt-5-mini
	          gpt-5
	          grok-code-fast-1
	          gemini-2.5-pro
	          raptor-mini)
	        "List of AI models available for Copilot.")
	      :custom
	      (gptel-model 'claude-sonnet-4.5)
	      (gptel-default-mode 'org-mode)
	      (gptel-use-curl t)
	      (gptel-use-tools t)
	      (gptel-confirm-tool-calls 'always)
	      (gptel-include-tool-results 'auto)
	      :config
	      (require 'gptel-integrations)
	      (require 'gptel-org)
	      (setq gptel--system-message 
	            (concat gptel--system-message " Make sure to use Chinese language. Note org-mode markup symbols need spaces with CJK characters including CJK symbols, such as ，。、「」 etc.."))
	      (setq gptel-backend 
	            (gptel-make-gh-copilot "Copilot"
	                                   :stream t
	                                   :models idiig/copilot-model-list)))
	    (use-package gptel-magit
	      :commands gptel-magit-generate-message
	      :custom
	      (gptel-magit-model 'grok-code-fast-1)
	      :hook (magit-mode . gptel-magit-install))
	    (use-package agent-shell
	      :demand t
	      :config
	      (add-to-list 'exec-path "${pkgs.claude-code-acp}/bin"))
	    (use-package meow
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
	    (require 'eaf)
	    (require 'eaf-browser)
	    (require 'eaf-pdf-viewer)
	    (add-to-list 'exec-path "${pkgs.wmctrl}/bin")
	    (setq eaf-webengine-default-zoom 2.0
	            eaf-browse-blank-page-url "https://www.kagi.com"
	            eaf-browser-auto-import-chrome-cookies nil   ; 非自动 cookies
	            eaf-browser-enable-autofill t                ; 自动填充密码
	            eaf-browser-enable-tampermonkey t)	     ; 使用油猴
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
            ctrlf
            wgrep
            puni
            ddskk
            migemo
            pyim
              pyim-basedict
            magit
            wanderlust
            spacious-padding
              writeroom-mode
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
            citre
            
            direnv
            eat
            nix-mode
            slime
              geiser                        # for scheme
            haskell-mode
            go-mode
            jq-mode
            auctex
              auctex-latexmk
            typst-ts-mode
              typst-preview
            jsonian
            plantuml-mode
            ob-nix
              ob-go
            org-bullets
            valign
            citeproc
            vulpea
            ox-reveal
            ox-typst
            org-present
            dslide
            org-modern
            copilot
            mcp
            gptel
              gptel-fn-complete
              gptel-magit
            agent-shell
            meow
              meow-tree-sitter
            (eaf.withApplications [ eaf-browser eaf-pdf-viewer ])
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

	      echo "alias ne='QT_QUICK_BACKEND=software LIBGL_ALWAYS_SOFTWARE=1 ${emacsWithPackages}/bin/emacs --init-dir \"$EMACS_DIR\"'" >> "$HOME/.bashrc"

	      # 提示用户手动 source 而不是直接执行，以避免 shell 继承问题
	      echo "请手动运行 'source ~/.bashrc' 以使 alias 生效"
	      echo "Emacs 配置已同步到 $EMACS_DIR"
	      '';  
	      });
}
