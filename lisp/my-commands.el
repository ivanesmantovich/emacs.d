;;; my-commands.el --- My custom interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; Collection of useful interactive commands

;;; Code:

(defun my/update-info-docs ()
  "Compile all .texi files to .info and update the dir file."
  (interactive)
  (let* ((packages-dir (expand-file-name "lisp/packages" user-emacs-directory))
         (info-dir (expand-file-name "info" packages-dir))
         (texi-files (directory-files-recursively packages-dir "\\.texi$")))
    
    ;; Create info directory if it doesn't exist
    (unless (file-exists-p info-dir)
      (make-directory info-dir t))
    
    ;; Process each .texi file
    (if (null texi-files)
        (message "No .texi files found in %s" packages-dir)
      (dolist (texi-file texi-files)
        (let* ((package-name (file-name-base texi-file))
               (info-file (expand-file-name (concat package-name ".info") info-dir)))
          
          (message "Processing: %s" texi-file)
          
          ;; Compile .texi to .info
          (if (zerop (call-process "makeinfo" nil nil nil 
                                   texi-file "-o" info-file))
              (progn
                ;; Update dir file
                (call-process "install-info" nil nil nil
                              (concat "--info-dir=" info-dir)
                              info-file)
                (message "✓ Installed: %s.info" package-name))
            (message "✗ Failed to compile: %s" texi-file))))
      
      (message "Done! Info files are in %s" info-dir))))

(defun my/kill-port (port)
  "Kill process running on PORT."
  (interactive "nKill process on port: ")
  (let* ((command (format "lsof -ti:%d 2>/dev/null" port))
         (pid (string-trim (shell-command-to-string command))))
    (if (string-empty-p pid)
        (message "No process found on port %d" port)
      (shell-command (format "kill -9 %s" pid))
      (message "Killed process %s on port %d" pid port))))

(provide 'my-commands)
;;; my-commands.el ends here
