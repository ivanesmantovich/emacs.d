;;; early-init.el --- Early initialization -*- lexical-binding: t -*-

;; gc optimization
(let ((default-gc-cons-threshold gc-cons-threshold))
  ;; increase during init
  (setq gc-cons-threshold most-positive-fixnum)
  ;; restore default after init
  (add-hook 'after-init-hook
            (lambda ()
              (setq gc-cons-threshold default-gc-cons-threshold))))

(setq mac-option-modifier 'meta
      mac-command-modifier 'super
      native-comp-async-report-warnings-errors 'silent
      byte-compile-warnings nil
      inhibit-splash-screen t
      make-backup-files nil
      ring-bell-function 'ignore
      custom-file (expand-file-name "~/.emacs.d/custom.el"))

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

;;; early-init.el ends here
