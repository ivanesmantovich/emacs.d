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
(blink-cursor-mode 0)

;; face
(set-face-attribute 'default nil :family "TX-02" :height 160) ; code
(set-fontset-font t 'cyrillic (font-spec :family "SF Mono") nil 'append) ; code, cyrillic fallback
(set-face-attribute 'variable-pitch nil :family "SF Pro Text" :height 160) ; org, prose
(set-face-attribute 'fixed-pitch nil :family "TX-02" :height 160) ; org, code inside of org

;; package
(require 'package)
(setq package-archives
      '(("gnu"   . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))
(require 'use-package-ensure)
(setq use-package-always-ensure t)

;; god-mode
(use-package god-mode
  :demand t
  :bind (("<escape>"   . god-local-mode)
         ("C-<escape>" . god-mode-all)
         ("C-x C-1"    . delete-other-windows)
         ("C-x C-2"    . split-window-below)
         ("C-x C-3"    . split-window-right)
         ("C-x C-0"    . delete-window))
  :bind (:map god-local-mode-map
              ("SPC" . repeat))
  :custom
  (god-mod-alist '((nil . "C-")
                   ("m" . "M-")
                   ("M" . "C-M-")))
  :config
  (defun handle-god-mode-toggle ()
    (set-cursor-color (if god-local-mode "orange" "black")))
  (add-hook 'god-mode-enabled-hook  #'handle-god-mode-toggle)
  (add-hook 'god-mode-disabled-hook #'handle-god-mode-toggle))

;; org
(add-hook 'org-mode-hook 'variable-pitch-mode)
(add-hook 'org-mode-hook 'visual-line-mode)
(custom-theme-set-faces
 'user
 '(org-code               ((t (:inherit fixed-pitch))))
 '(org-verbatim           ((t (:inherit fixed-pitch))))
 '(org-table              ((t (:inherit fixed-pitch))))
 '(org-block              ((t (:inherit fixed-pitch))))
 '(org-block-begin-line   ((t (:inherit fixed-pitch))))
 '(org-block-end-line     ((t (:inherit fixed-pitch))))
 '(org-formula            ((t (:inherit fixed-pitch))))
 '(org-checkbox           ((t (:inherit fixed-pitch))))
 '(org-meta-line          ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-special-keyword    ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-indent             ((t (:inherit (org-hide fixed-pitch)))))) ; keep indent guides aligned with fixed width
 '(org-document-info-keyword ((t (:inherit (shadow fixed-pitch)))))

;; dired
(put 'dired-find-alternate-file 'disabled nil) ; enable alternate command, that replaces the current buffer
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages '(god-mode)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-block ((t (:inherit fixed-pitch))))
 '(org-block-begin-line ((t (:inherit fixed-pitch))))
 '(org-block-end-line ((t (:inherit fixed-pitch))))
 '(org-checkbox ((t (:inherit fixed-pitch))))
 '(org-code ((t (:inherit fixed-pitch))))
 '(org-formula ((t (:inherit fixed-pitch))))
 '(org-indent ((t (:inherit (org-hide fixed-pitch)))))
 '(org-meta-line ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-special-keyword ((t (:inherit (font-lock-comment-face fixed-pitch)))))
 '(org-table ((t (:inherit fixed-pitch))))
 '(org-verbatim ((t (:inherit fixed-pitch)))))
