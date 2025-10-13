;;; my-commands.el --- My custom interactive commands -*- lexical-binding: t -*-

;;; Commentary:
;; Collection of useful interactive commands

;;; Code:

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
