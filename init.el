;;; init.el --- Happy hacking! -*- lexical-binding: t -*-

;; redirect auto-saves
(defvar auto-save-dir (concat user-emacs-directory "auto-saved-files/"))
(make-directory auto-save-dir t)
(setq auto-save-file-name-transforms `((".*" ,auto-save-dir t)))

(load custom-file 'noerror)

;; packages
(mapc #'require '(compat		; dependencies
		  dash
		  s
		  f
		  transient
		  llama
		  with-editor

		  info			; built-in
		  compile
		  ansi-color

		  elisp-demos		; community
                  helpful
                  reverse-im
                  modus-themes
                  magit
		  avy
                  diff-hl
                  prodigy
                  html-ts-mode          ; note: will be built-in in Emacs 30
                  cape
                  eglot
                  eglot-booster         ; todo: try disabling this after upgrading to Emacs 30
                  corfu
                  corfu-popupinfo
                  vertico
                  vertico-multiform
                  vertico-sort
                  consult
                  consult-compile
                  orderless
                  marginalia
                  embark
                  embark-consult
                  which-key             ; note: will be built-in in Emacs 30
                  diminish

		  my-prodigy-modifications

		  seashell		; my packages
		  my-commands))
;; TODO: extract helpers and dependencies like elisp-demos.org or f.el/s.el into subdir
;; TODO: flymake-jsts has biome and eslint support and afaik easily customzible to add support for other tools

;; package info
(add-to-list 'Info-directory-list (expand-file-name "lisp/packages/info" user-emacs-directory))

;; package values
(setq recentf-max-saved-items 500
      
      dired-use-ls-dired t
      dired-listing-switches "-aoh --group-directories-first"
      
      reverse-im-input-methods '("russian-computer")
      reverse-im-cache-file (locate-user-emacs-file "reverse-im-cache.el")

      org-image-actual-width 500
      org-startup-with-inline-images t

      dabbrev-case-fold-search t
      dabbrev-case-replace nil

      which-key-idle-delay 0.5
      which-key-popup-type 'minibuffer

      avy-timeout-seconds 0.3

      magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1

      eglot-autoshutdown t

      corfu-auto t	       
      corfu-auto-delay 0.1     
      corfu-popupinfo-delay 0.3
      corfu-quit-no-match t

      enable-recursive-minibuffers t
      vertico-cycle t
      vertico-count 20
      vertico-multiform-commands '((execute-extended-command
				    (vertico-sort-function . vertico-sort-history-length-alpha)))

      xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref

      completion-styles '(orderless basic)
      completion-category-overrides '((file (styles partial-completion))))

;; theme
(load-theme 'modus-operandi-tinted :no-confirm)

;; modes
(blink-cursor-mode 0)
(scroll-bar-mode -1)
(repeat-mode 1)
(electric-pair-mode 1)
(savehist-mode 1)
(recentf-mode 1)
(global-so-long-mode 1)
(reverse-im-mode t)
(global-diff-hl-mode)
(eglot-booster-mode)
(global-corfu-mode)
(corfu-popupinfo-mode)
(vertico-mode)
(vertico-multiform-mode)
(marginalia-mode)
(which-key-mode)
(seashell-minor-mode 1)

;; file-based major modes
(add-to-list 'auto-mode-alist '("\\.html\\'" . html-ts-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . css-ts-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . js-ts-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-ts-mode))

;; hooks
(add-hook 'org-mode-hook 'visual-line-mode)
(add-hook 'dired-mode-hook 'dired-hide-details-mode) ;  press ( to toggle details
(add-hook 'shell-mode-hook 'compilation-shell-minor-mode)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)
(add-hook 'typescript-ts-mode-hook 'eglot-ensure)
(add-hook 'tsx-ts-mode-hook 'eglot-ensure)
(add-hook 'js-ts-mode-hook 'eglot-ensure)
(add-hook 'css-ts-mode-hook 'eglot-ensure) ; maybe use regular non-ts css and html modes
(add-hook 'html-ts-mode-hook 'eglot-ensure)
(add-hook 'embark-collect-mode-hook 'consult-preview-at-point-mode)

(add-hook 'prodigy-view-mode-hook (lambda () (compilation-minor-mode 1)))

;; diminish modes
(mapc #'diminish '(which-key-mode
		   auto-revert-mode
		   eldoc-mode))

;; advices
(advice-add 'describe-function-1 :after #'elisp-demos-advice-describe-function-1)
(advice-add 'helpful-update :after #'elisp-demos-advice-helpful-update)

;; allow disabled commands 
(put 'dired-find-alternate-file 'disabled nil)

;; face
(set-face-attribute 'default nil :family "TX-02" :height 180)
(set-fontset-font t 'cyrillic (font-spec :family "SF Mono") nil 'append) ; fallback
(custom-set-faces
   '(org-level-1 ((t (:family "Alegreya" :height 220))))
   '(org-level-2 ((t (:family "Alegreya" :height 210))))
   '(org-level-3 ((t (:family "Alegreya" :height 200))))
   '(org-block ((t (:family "TX-02" :height 160))))
   '(org-code ((t (:family "TX-02" :height 160))))
   '(org-verbatim ((t (:family "TX-02" :height 160)))))
(add-hook 'org-mode-hook (lambda ()
			   (face-remap-add-relative 'default :family "Alegreya" :height 200)))

;; treesitter
(setq treesit-language-source-alist
      '((typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (css "https://github.com/tree-sitter/tree-sitter-css" "master" "src")
        (html "https://github.com/tree-sitter/tree-sitter-html" "v0.20.1" "src")))

;; TODO: enable it in all modes, not just CSS
(add-hook 'css-ts-mode-hook
          (lambda ()
            (add-hook 'completion-at-point-functions #'cape-dabbrev 90 t)))

;; modifications
;; consult.el
(consult-customize
 consult-fd
 consult-find
 consult-ripgrep
 consult-grep
 consult-git-grep
 consult-buffer
 :preview-key '(:debounce 0.25 any))

;; xref.el
(add-to-list 'display-buffer-alist '("\\*xref\\*"
				     (display-buffer-at-bottom)
				     (window-height . 0.25)))

;; project.el
(defun my/project-try-local (dir)
  "Check if DIR contains a .project file."
  (let ((root (locate-dominating-file dir ".project"))) ; TODO: use dir-locals.el instead of .project
    (when root
      (cons 'transient root))))
(add-hook 'project-find-functions #'my/project-try-local nil nil) ;; add to the beginning of project-find-functions

;; compile.el
(dolist (pattern '((rsbuild-typescript
                      "^ERROR in \\([^\n]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                      1 2 3 2)
                     (angular-webpack
                      "^Error: \\([^:\n]+\\):\\([0-9]+\\):\\([0-9]+\\)"
                      1 2 3 2)))
    (add-to-list 'compilation-error-regexp-alist-alist pattern)
    (add-to-list 'compilation-error-regexp-alist (car pattern)))
(add-hook 'compilation-filter-hook 'ansi-color-compilation-filter)

;; note to self: use deepwiki and claude to understand any package
