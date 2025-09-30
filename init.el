(defun my/add-to-paths (dir)
  "Add DIR to both exec-path and PATH environment variable if it exists."
  (when (file-directory-p dir)
    (add-to-list 'exec-path dir)
    (let ((path (getenv "PATH")))
      (unless (string-match-p (regexp-quote dir) path)
        (setenv "PATH" (concat dir ":" path))))))

(my/add-to-paths "/opt/homebrew/opt/coreutils/libexec/gnubin")
;; (my/add-to-path "/opt/homebrew/opt/findutils/libexec/gnubin") ; TODO do i need findutils?
;; (my/add-to-path "/opt/homebrew/opt/gnu-sed/libexec/gnubin")

;; setup
(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'super)
(setq native-comp-async-report-warnings-errors 'silent)
(setq byte-compile-warnings nil)
(setq make-backup-files nil)
(setq ring-bell-function 'ignore)
(blink-cursor-mode 0)

;; face
(set-face-attribute 'default nil :family "TX-02" :height 160) ; code
(set-fontset-font t 'cyrillic (font-spec :family "SF Mono") nil 'append) ; code, cyrillic fallback

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
  :vc (:url "https://github.com/protesilaos/modus-themes.git"
       :rev "4.8.0")
  :config
;   (load-theme 'modus-operandi-tinted :no-confirm))
  (load-theme 'modus-vivendi-tinted :no-confirm))

;; TODO: use alegreya in org https://fonts.google.com/specimen/Alegreya
;; org
(add-hook 'org-mode-hook 'visual-line-mode)
(setq org-image-actual-width 800)

;; dired
(put 'dired-find-alternate-file 'disabled nil) ; enable alternate command, that replaces the current buffer

;; redirect custom
(setq custom-file (expand-file-name "~/.emacs.d/custom.el"))
(load custom-file 'noerror)
