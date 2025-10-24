;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(let ((packages-dir (expand-file-name "lisp/packages" user-emacs-directory)))
  (push packages-dir load-path)
  (let ((default-directory packages-dir))
    (normal-top-level-add-subdirs-to-load-path)))

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
(my/add-to-paths "~/.cargo/bin")
;; (my/add-to-path "/opt/homebrew/opt/findutils/libexec/gnubin") ; TODO do i need findutils?
;; (my/add-to-path "/opt/homebrew/opt/gnu-sed/libexec/gnubin")

(add-to-list 'default-frame-alist '(fullscreen . fullscreen))

;; setup
(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings nil
      inhibit-splash-screen t
      make-backup-files nil
      ring-bell-function 'ignore
      gc-cons-threshold (* 100 1024 1024) ; gc tweak, keep it simple. gcmh package is an alternative
      custom-file (expand-file-name "~/.emacs.d/custom.el"))

;;; early-init.el ends here
