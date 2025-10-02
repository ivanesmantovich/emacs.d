;;; seashell.el --- Personal keybindings -*- lexical-binding: t -*-

;;; Commentary:
;; My personal keybindings that override all modes.

;;; Code:

(defun my/smart-beginning-of-line ()
  "Move to indentation, or if already there, to beginning of line."
  (interactive "^")
  (let ((oldpos (point)))
    (back-to-indentation)
    (when (= oldpos (point))
      (move-beginning-of-line nil))))

(defun my/kill-to-indentation ()
  "Kill from cursor back to indentation.
If already at indentation, delete the indentation without saving to kill ring."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (if (= prev-pos (point))
        (delete-region (line-beginning-position) (point))
      (kill-region (point) prev-pos))))


(defvar seashell-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-e") 'eval-buffer)
    (define-key map (kbd "C-o") 'other-window)
    (define-key map (kbd "C-,") 'switch-to-buffer)
    (define-key map (kbd "M-s-w") 'duplicate-dwim)
    (define-key map (kbd "C-s-k") 'kill-whole-line)

    (define-key map (kbd "M-<left>") 'backward-word)
    (define-key map (kbd "M-<right>") 'forward-word)
    (define-key map (kbd "M-<up>") 'backward-paragraph)
    (define-key map (kbd "M-<down>") 'forward-paragraph)
    (define-key map (kbd "s-<left>") 'my/smart-beginning-of-line)
    (define-key map (kbd "s-<right>") 'end-of-line)
    (define-key map (kbd "s-<up>") 'beginning-of-buffer)
    (define-key map (kbd "s-<down>") 'end-of-buffer)
    (define-key map (kbd "s-<backspace>") 'my/kill-to-indentation)
    map)
  "My personal keybindings that override all modes.")

(define-minor-mode seashell-minor-mode
  "A minor mode for my personal keybindings that override all modes."
  :init-value t
  :lighter ""
  :keymap seashell-minor-mode-map)

(add-to-list 'emulation-mode-map-alists
	     `((seashell-minor-mode . ,seashell-minor-mode-map)))

(provide 'seashell)
