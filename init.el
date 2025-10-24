;;; init.el --- Happy hacking! -*- lexical-binding: t -*-

(add-to-list 'Info-directory-list (expand-file-name "lisp/packages/info" user-emacs-directory))

(add-to-list 'default-frame-alist '(fullscreen . fullscreen)) ; start in fullscreen

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(repeat-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-so-long-mode 1)

;; setup
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq native-comp-async-report-warnings-errors 'silent)      ; silence byte-compilation
(setq byte-compile-warnings nil)                             ; silence byte-compilation
(setq inhibit-splash-screen t)
(setq make-backup-files nil)		                     ; disable backup
(setq ring-bell-function 'ignore)	                     ; disable bell
(setq enable-recursive-minibuffers t)
(setq gs-cons-threshold (* 100 1024 1024))                   ; gc tweak, keep it simple. gcmh package is an alternative
(setq recentf-max-saved-items 500)
(setq custom-file (expand-file-name "~/.emacs.d/custom.el")) ; redirect custom
(load custom-file 'noerror)				     ; redirect custom

;; redirect auto-saves
(defvar auto-save-dir
  (concat user-emacs-directory "auto-saved-files/"))
(make-directory auto-save-dir t)
(setq auto-save-file-name-transforms
      `((".*" ,auto-save-dir t)))

;; face
(set-face-attribute 'default nil :family "TX-02" :height 180)
(set-fontset-font t 'cyrillic (font-spec :family "SF Mono") nil 'append) ; fallback

;; note to self: use deepwiki and claude to understand any package

;; dependencies
(require 'compat)
(require 'dash)
(require 's)
(require 'f)
(require 'transient)
(require 'cond-let)
(require 'llama)
(require 'with-editor)

;; packages
(require 'elisp-demos)
(require 'helpful)
(require 'reverse-im)
(require 'modus-themes)
(require 'magit)
(require 'diff-hl)
(require 'prodigy)
(require 'html-ts-mode)			; will be built-in in Emacs 30
(require 'cape)
(require 'eglot)
(require 'eglot-booster)		; try disabling this after upgrading to Emacs 30
(require 'corfu)
(require 'corfu-popupinfo)
(require 'vertico)
(require 'vertico-multiform)
(require 'vertico-sort)
(require 'consult)
(require 'consult-compile)
(require 'orderless)
(require 'marginalia)
(require 'embark)
(require 'embark-consult)
(require 'which-key)			; will be built-in in Emacs 30
(require 'diminish)
;; TODO: extract helpers and dependencies like elisp-demos.org or f.el/s.el into subdir
;; TODO: flymake-jsts has biome and eslint support and afaik easily customzible to add support for other tools

;; my package modifications
(require 'my-prodigy-modifications)

;; my packages and modes
(require 'seashell)
(require 'my-commands)

(advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; reverse-im
(setq reverse-im-input-methods '("russian-computer")
      reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
(reverse-im-mode t) 

;; modus-theme
(load-theme 'modus-operandi-tinted :no-confirm)

;; magit
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; diff-hl
(global-diff-hl-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; avy
(require 'avy)
(setq avy-timeout-seconds 0.3)

;; dired
(put 'dired-find-alternate-file 'disabled nil) ; enable alternate command, that replaces the current buffer
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ;  press ( to toggle details
(setq dired-use-ls-dired t)
(setq dired-listing-switches "-aoh --group-directories-first")

;; org
(setq org-image-actual-width 500
      org-startup-with-inline-images t)
(custom-set-faces
   '(org-level-1 ((t (:family "Alegreya" :height 220))))
   '(org-level-2 ((t (:family "Alegreya" :height 210))))
   '(org-level-3 ((t (:family "Alegreya" :height 200))))
   '(org-block ((t (:family "TX-02" :height 160))))
   '(org-code ((t (:family "TX-02" :height 160))))
   '(org-verbatim ((t (:family "TX-02" :height 160)))))
(add-hook 'org-mode-hook (lambda ()
			   (face-remap-add-relative 'default :family "Alegreya" :height 200)))
(add-hook 'org-mode-hook 'visual-line-mode)

;; treesitter
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (css "https://github.com/tree-sitter/tree-sitter-css" "master" "src")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.1" "src")))
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode))

;; eglot
(setq eglot-autoshutdown t)
(setq eglot-events-buffer-size 0)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'css-ts-mode-hook 'eglot-ensure) ; maybe use regular non-ts css and html modes
(add-hook 'html-ts-mode-hook 'eglot-ensure)
;; TODO: CSS variables language server
;; $ npm install -g css-variables-language-server
;; $ css-variables-language-server --stdio
(eglot-booster-mode)

;; dabbrev
(require 'dabbrev)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)

;; TODO: enable it in all modes, not just CSS
(add-hook 'css-ts-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions #'cape-dabbrev 90 t)))

;; corfu
(setq corfu-auto t
      corfu-auto-delay 0.1
      corfu-popupinfo-delay 0.3
      corfu-quit-no-match t)
(global-corfu-mode)
(corfu-popupinfo-mode)

;; vertico
(setq vertico-cycle t
      vertico-count 20
      vertico-multiform-commands '((execute-extended-command
				    (vertico-sort-function . vertico-sort-history-length-alpha))))
(vertico-mode)
(vertico-multiform-mode)

;; consult
(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)
(consult-customize
 consult-fd
 consult-find
 consult-ripgrep
 consult-grep
 consult-git-grep
 :preview-key '(:debounce 0.25 any))

;; embark
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

;; orderless
(setq completion-styles '(orderless basic))
(setq completion-category-overrides '((file (styles partial-completion))))

;; marginalia
(marginalia-mode)

;; prodigy
(add-hook 'prodigy-view-mode-hook
	  (lambda ()
	    (compilation-minor-mode 1)))

;; which-key
(setq which-key-idle-delay 0.5
      which-key-popup-type 'minibuffer)
(which-key-mode)

;; diminish
(mapc #'diminish '(which-key-mode
		   auto-revert-mode
		   eldoc-mode))

;; xref
(add-to-list 'display-buffer-alist '("\\*xref\\*"
				     (display-buffer-at-bottom)
				     (window-height . 0.25)))

;; project.el modifications
(defun my-project-try-local (dir)
  "Check if DIR contains a .project file."
  (let ((root (locate-dominating-file dir ".project"))) ; TODO: use dir-locals.el instead of .project
    (when root
      (cons 'transient root))))
(add-hook 'project-find-functions #'my-project-try-local nil nil) ;; add to the beginning of project-find-functions

;; shell.el modifications
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; compile.el modifications
(with-eval-after-load 'compile
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rsbuild-typescript
                 "^ERROR in \\([^
]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3 2))
  (add-to-list 'compilation-error-regexp-alist 'rsbuild-typescript)
  (add-to-list 'compilation-error-regexp-alist-alist
               '(angular-webpack
                 "^Error: \\([^:
]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3 2))
  (add-to-list 'compilation-error-regexp-alist 'angular-webpack)
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

(seashell-minor-mode 1)
