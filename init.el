;;; -*- lexical-binding: t -*-

(defun my/add-to-paths (dir)
  "Add DIR to both exec-path and PATH environment variable if it exists."
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (let ((path (getenv "PATH")))
      (unless (string-match-p (regexp-quote dir) path)
        (setenv "PATH" (concat dir ":" path))))))

(my/add-to-paths "/opt/homebrew/bin")
(my/add-to-paths (expand-file-name "~/.local/share/fnm/aliases/default/bin")) ; fnm bin dir
(my/add-to-paths "/opt/homebrew/opt/coreutils/libexec/gnubin")
;; (my/add-to-path "/opt/homebrew/opt/findutils/libexec/gnubin") ; TODO do i need findutils?
;; (my/add-to-path "/opt/homebrew/opt/gnu-sed/libexec/gnubin")
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(add-to-list 'default-frame-alist '(fullscreen . fullscreen)) ; start in fullscreen

(blink-cursor-mode 0)
(scroll-bar-mode -1)
(repeat-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)
(recentf-mode 1)

;; setup
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq native-comp-async-report-warnings-errors 'silent)      ; silence byte-compilation
(setq byte-compile-warnings nil)                             ; silence byte-compilation
(setq make-backup-files nil)		                     ; disable backup
(setq ring-bell-function 'ignore)	                     ; disable bell
(setq enable-recursive-minibuffers t)
(setq gs-cons-threshold (* 100 1024 1024))                   ; gc tweak, keep it simple. gcmh package is an alternative
(setq recentf-max-saved-items 500)
(setq custom-file (expand-file-name "~/.emacs.d/custom.el")) ; redirect custom
(load custom-file 'noerror)				     ; redirect custom

;; face
(set-face-attribute 'default nil :family "TX-02" :height 170)
(set-fontset-font t 'cyrillic (font-spec :family "SF Mono") nil 'append) ; fallback

;; note to self: use deepwiki and claude to understand any package. happy hacking!

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

(use-package modus-themes
  :vc (:url "https://github.com/protesilaos/modus-themes"
       :rev "4.8.0")
  :config
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package magit
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh))

(use-package reverse-im
  :demand t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  :config
  (reverse-im-mode t))

(use-package avy)

;; dired
(put 'dired-find-alternate-file 'disabled nil) ; enable alternate command, that replaces the current buffer

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
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay 0.5)
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

;; TODO: integrate consult-compile-error and consult-flymake as well https://github.com/minad/consult?tab=readme-ov-file#compilation
;; TODO: create a separate command with --base passed to consult-fd-args to search only for filenames.
(use-package consult
  :vc (:url "https://github.com/minad/consult"
	    :rev "2.8")
  :config
  (require 'consult-compile)
  (setq xref-show-xrefs-function #'consult-xref ; use consult to view xref locations
	xref-show-definitions-function #'consult-xref))

(use-package embark)

(use-package embark-consult
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

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

(use-package prodigy
  :config
  (add-hook 'prodigy-view-mode-hook
	    (lambda ()
	      (compilation-minor-mode 1)))
  (require 'my-prodigy-tweaks))

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

;; project.el tweaks
(defun my-project-try-local (dir)
  "Check if DIR contains a .project file."
  (let ((root (locate-dominating-file dir ".project")))
    (when root
      (cons 'transient root))))
(add-hook 'project-find-functions #'my-project-try-local nil nil) ;; add to the beginning of project-find-functions

;; shell.el tweaks
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)

;; compile.el tweaks
(with-eval-after-load 'compile
  ;; RSBuild/Webpack TypeScript errors
  (add-to-list 'compilation-error-regexp-alist-alist
               '(rsbuild-typescript
                 "^ERROR in \\([^
]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                 1 2 3 2))
  (add-to-list 'compilation-error-regexp-alist 'rsbuild-typescript)
  (require 'ansi-color)
  (add-hook 'compilation-filter-hook 'ansi-color-compilation-filter))

;; my modes
(require 'seashell)
(seashell-minor-mode 1)
