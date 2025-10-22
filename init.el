;;; -*- lexical-binding: t -*-

;; Happy hacking!

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
(require 'reverse-im)
(require 'modus-themes)
(require 'magit)
(require 'diff-hl)
(require 'prodigy)
(require 'cape)

;; my package modifications
(require 'my-prodigy-modifications)

;; my packages and modes
(require 'seashell)
(require 'my-commands)

;; reverse-im
(setq reverse-im-input-methods '("russian-computer")
      reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
(reverse-im-mode t) 

;; modus-theme
(load-theme 'modus-vivendi-tinted :no-confirm)

;; magit
(setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

;; diff-hl
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

;; package
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(setq package-install-upgrade-built-in t)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; avy
(require 'avy)
(setq avy-timeout-seconds 0.3)

;; dired
(put 'dired-find-alternate-file 'disabled nil) ; enable alternate command, that replaces the current buffer
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ;  press ( to toggle details
(setq dired-use-ls-dired t)
(setq dired-listing-switches "-aoh --group-directories-first")

;; org
(defun my-org-font-setup ()
  "Use Alegreya in org-mode buffers."
  (face-remap-add-relative 'default :family "Alegreya" :height 200))
(use-package org
  :hook ((org-mode . my-org-font-setup)
	 (org-mode . visual-line-mode))
  :custom
  (org-image-actual-width 800)
  (org-startup-with-inline-images t)
  :config
  (custom-set-faces
   '(org-level-1 ((t (:family "Alegreya" :height 220))))
   '(org-level-2 ((t (:family "Alegreya" :height 210))))
   '(org-level-3 ((t (:family "Alegreya" :height 200))))
   '(org-block ((t (:family "TX-02" :height 160))))
   '(org-code ((t (:family "TX-02" :height 160))))
   '(org-verbatim ((t (:family "TX-02" :height 160))))))

;; treesitter
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (css "https://github.com/tree-sitter/tree-sitter-css" "master" "src")
        (html "https://github.com/tree-sitter/tree-sitter-html" "master" "src")))
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
(add-hook 'html-mode-hook 'eglot-ensure)
(add-hook 'css-mode-hook 'eglot-ensure)
(add-hook 'css-ts-mode-hook 'eglot-ensure)

;; NOTE: Try disabling this after upgrading to Emacs 30
(use-package eglot-booster
  :vc (:url "https://github.com/jdtsmith/eglot-booster")
  :after eglot
  :config (eglot-booster-mode))

;; dabbrev
(require 'dabbrev)
(setq dabbrev-case-fold-search t)
(setq dabbrev-case-replace nil)

;; TODO: enable it in all modes, not just CSS
(add-hook 'css-ts-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions #'cape-dabbrev 90 t)))

(use-package corfu
  :vc (:url "https://github.com/minad/corfu"
       :rev "2.3")
  :init
  (global-corfu-mode)
  (add-to-list 'load-path (expand-file-name "corfu/extensions" package-user-dir))
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.1)
  (corfu-popupinfo-delay 0.3)
  (corfu-quit-no-match t))

(use-package vertico
  :vc (:url "https://github.com/minad/vertico"
       :rev "2.5")
  :custom
  (vertico-cycle t)
  (vertico-count 20)
  :init
  (vertico-mode)
  :config
  (add-to-list 'load-path (expand-file-name "vertico/extensions" package-user-dir))
  (require 'vertico-multiform)
  (require 'vertico-sort)
  (vertico-multiform-mode)
  (setq vertico-multiform-commands
	'((execute-extended-command
	   (vertico-sort-function . vertico-sort-history-length-alpha)))))

(use-package consult
  :vc (:url "https://github.com/minad/consult"
	    :rev "2.8")
  :config
  (require 'consult-compile))

;; consult
(consult-customize
 consult-fd
 consult-find
 consult-ripgrep
 consult-grep
 consult-git-grep
 :preview-key '(:debounce 0.25 any))

(setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref)

;; (use-package embark)

;; (use-package embark-consult
;;   :hook
;;   (embark-collect-mode . consult-preview-at-point-mode))

(use-package orderless
  :vc (:url "https://github.com/oantolin/orderless"
       :rev "1.5")
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))
 
(use-package marginalia
  :vc (:url "https://github.com/minad/marginalia"
       :rev "2.3")
  :init
  (marginalia-mode))

(add-hook 'prodigy-view-mode-hook
	  (lambda ()
	    (compilation-minor-mode 1)))

;; TODO: which-key is built-in in Emacs 30
(use-package which-key
  :config
  (which-key-mode 1)
  (setq which-key-idle-delay 0.5)
  (setq which-key-popup-type 'minibuffer))

(use-package diminish
  :config
  (diminish 'which-key-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode))

;; show xref at the bottom
(add-to-list 'display-buffer-alist
             '("\\*xref\\*"
               (display-buffer-at-bottom)
               (window-height . 0.25)))

;; project.el modifications
(defun my-project-try-local (dir)
  "Check if DIR contains a .project file."
  (let ((root (locate-dominating-file dir ".project")))
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
