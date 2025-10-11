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

;; Translate QMK firmware-level keys back to Emacs native bindings
(define-key key-translation-map (kbd "<up>") (kbd "C-p"))
(define-key key-translation-map (kbd "<down>") (kbd "C-n"))
(define-key key-translation-map (kbd "<left>") (kbd "C-b"))
(define-key key-translation-map (kbd "<right>") (kbd "C-f"))
(define-key key-translation-map (kbd "s-<left>") (kbd "C-a"))
(define-key key-translation-map (kbd "s-<right>") (kbd "C-e"))
(define-key key-translation-map (kbd "<deletechar>") (kbd "C-d"))
(define-key key-translation-map (kbd "<next>") (kbd "C-v"))
(define-key key-translation-map (kbd "<prior>") (kbd "M-v"))
(define-key key-translation-map (kbd "M-<left>") (kbd "M-b"))
(define-key key-translation-map (kbd "M-<right>") (kbd "M-f"))
(define-key key-translation-map (kbd "s-x") (kbd "C-w"))
(define-key key-translation-map (kbd "s-c") (kbd "M-w"))
(define-key key-translation-map (kbd "s-v") (kbd "C-y"))

;; Keymaps that I need to work anywhere.
;; If I override a mapping from some mode that i want to use, then I will map it to my liking.
;; Otherwise, I never need to worry about keymap conflicts.
(defvar seashell-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "s-e") 'eval-buffer)
    (define-key map (kbd "s-o") 'other-window)
    (define-key map (kbd "s-w") 'duplicate-dwim)
    (define-key map (kbd "s-k") 'kill-whole-line)
    (define-key map (kbd "C-a") 'my/smart-beginning-of-line)
    (define-key map (kbd "s-<backspace>") 'my/kill-to-indentation)
    
    (define-key map (kbd "s-g") 'magit-status)
       
    (define-key map (kbd "C-;") 'avy-goto-char)
    (define-key map (kbd "C-'") 'avy-goto-char-2)
    (define-key map (kbd "M-g l") 'avy-goto-line)
    
    (define-key map (kbd "C-,") 'consult-buffer)
    (define-key map (kbd "M-s f") 'consult-fd)
    (define-key map (kbd "M-s g") 'consult-ripgrep)
    (define-key map (kbd "M-g r") 'consult-recent-file)
    
    (define-key map (kbd "C-.") 'embark-act)
    (define-key map (kbd "M-.") 'embark-dwim)
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
