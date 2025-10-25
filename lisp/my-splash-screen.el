(defun my/splash-screen ()
  "Display custom centered text."
  (let ((splash-buffer (get-buffer-create "*splash*")))
    (with-current-buffer splash-buffer
      (erase-buffer)
      (let* ((text "Reducing stress")
             (lines (split-string text "\n"))
             (max-width (apply 'max (mapcar 'length lines)))
             (window-width (window-width))
             (window-height (window-height))
             (vertical-padding (/ (- window-height (length lines)) 2)))
        
        (insert (make-string (max 0 vertical-padding) ?\n))
        
        (dolist (line lines)
          (let ((padding (/ (- window-width (length line)) 2)))
            (insert (make-string (max 0 padding) ?\s))
            (insert line "\n"))))
      
      (read-only-mode 1)
      (goto-char (point-min)))
    (switch-to-buffer splash-buffer)))

(provide 'my-splash-screen)
