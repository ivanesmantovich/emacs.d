;;; my-prodigy-modifications.el --- My Prodigy modifications -*- lexical-binding: t -*-

;;; Code:

(require 'prodigy)

(prodigy-define-service
  :name "Talk Frontend"
  :command "npm"
  :args '("run" "start")
  :env '(("CI" "true"))
  :cwd "~/Developer/work/talk/frontend"
  :tags '(work frontend webpack)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(prodigy-define-service
  :name "Stream Frontend"
  :command "yarn"
  :args '("start")
  :cwd "~/Developer/work/stream/frontend/app"
  :tags '(work frontend rsbuild)
  :stop-signal 'sigkill
  :kill-process-buffer-on-stop t)

(defun my/get-prodigy-buffer-name (service)
  "Get the buffer name for a Prodigy SERVICE."
  (let ((service-name (plist-get service :name)))
    (format "*prodigy-%s*" 
            (downcase (replace-regexp-in-string " " "-" service-name)))))

(defun my/clear-prodigy-buffer-hook (tag pattern)
  "Create a hook function that clears buffer for services with TAG when PATTERN matches output."
  (lambda (service output)
    (when (and (member tag (plist-get service :tags))
               (string-match-p pattern output))
      (when-let ((buf (get-buffer (my/get-prodigy-buffer-name service))))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (erase-buffer)))))))

;; Clear RSBuild buffers on new builds
(add-hook 'prodigy-process-on-output-hook
          (my/clear-prodigy-buffer-hook 'rsbuild "^start   "))

;; Clear Webpack buffers on new builds
(add-hook 'prodigy-process-on-output-hook
          (my/clear-prodigy-buffer-hook 'webpack "Browser application bundle generation complete"))

(defun my/prodigy-start-and-show ()
  "Start service and display its output after a short delay."
  (interactive)
  (let* ((service (prodigy-service-at-pos))
         (buffer-name (my/get-prodigy-buffer-name service)))
    (prodigy-start)
    (run-with-timer 1.0 nil
                    (lambda ()
                      (when-let ((buf (get-buffer buffer-name)))
                        (with-current-buffer buf
                          (unless (eq major-mode 'prodigy-view-mode)
                            (prodigy-view-mode)))
                        (save-selected-window
                          (display-buffer buf)))))))

(define-key prodigy-mode-map "s" 'my/prodigy-start-and-show)

(provide 'my-prodigy-modifications)
;;; my-prodigy-modifications.el ends here
