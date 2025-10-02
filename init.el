(defun my/add-to-paths (dir)
  "Add DIR to both exec-path and PATH environment variable if it exists."
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (let ((path (getenv "PATH")))
      (unless (string-match-p (regexp-quote dir) path)
        (setenv "PATH" (concat dir ":" path))))))

(my/add-to-paths "/opt/homebrew/bin")
(my/add-to-paths "/opt/homebrew/opt/coreutils/libexec/gnubin")
;; (my/add-to-path "/opt/homebrew/opt/findutils/libexec/gnubin") ; TODO do i need findutils?
;; (my/add-to-path "/opt/homebrew/opt/gnu-sed/libexec/gnubin")

;; setup
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq native-comp-async-report-warnings-errors 'silent)      ; silence byte-compilation
(setq byte-compile-warnings nil)                             ; silence byte-compilation
(setq make-backup-files nil)		                     ; disable backup
(setq ring-bell-function 'ignore)	                     ; disable bell
(blink-cursor-mode 0)					     ; disable blinking
(setq custom-file (expand-file-name "~/.emacs.d/custom.el")) ; redirect custom
(load custom-file 'noerror)				     ; redirect custom
(repeat-mode 1)						     ; enable repeat

(setq gs-cons-threshold (* 100 1024 1024))

(global-set-key (kbd "s-e") 'eval-buffer)

;; face
(set-face-attribute 'default nil :family "TX-02" :height 160)
(set-fontset-font t 'cyrillic (font-spec :family "SF Mono") nil 'append) ; fallback

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

;; Replace gs-cons-threshold with this if necessary
;; (use-package gcmh
;;   :demand t
;;   :config
;;   (gcmh-mode 1))

(use-package modus-themes
  :vc (:url "https://github.com/protesilaos/modus-themes"
       :rev "4.8.0")
  :config
;   (load-theme 'modus-operandi-tinted :no-confirm))
  (load-theme 'modus-vivendi-tinted :no-confirm))

(use-package magit
  :bind (("s-g" . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

(use-package reverse-im
  :demand t
  :custom
  (reverse-im-input-methods '("russian-computer"))
  (reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el"))
  :config
  (reverse-im-mode t))

(use-package avy
  :bind (("C-;" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g f" . avy-goto-line)
         ("M-g w" . avy-goto-word-1)))

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

(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (css "https://github.com/tree-sitter/tree-sitter-css" "master" "src")
        (html "https://github.com/tree-sitter/tree-sitter-html" "master" "src")))

;; treesitter
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
  :config
  (add-to-list 'load-path (expand-file-name "corfu/extensions" package-user-dir))
  (require 'corfu-popupinfo)
  (corfu-popupinfo-mode)
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.2)
  (corfu-popupinfo-delay 0.5)
  (corfu-quit-no-match t))
