;;; dumbparens.el --- Upload sharing to 0x0.st -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2023 William Vaughn
;;
;; Author: William Vaughn <vaughnwilld@gmail.com>
;; Maintainer: William Vaughn <vaughnwilld@gmail.com>
;; Created: June 26, 2021
;; Modified: August 16, 2023
;; Version: 1.0.1
;; Homepage: https://git.sr.ht/~willvaughn/emacs-0x0
;; Package-Requires: ((emacs "26.1"))
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Integration with https://0x0.st, envs.sh, ttm.sh, and self-hosted services
;;
;; Upload whatever you need to a pastebin server. The commands
;; `0x0-upload-file',`0x0-upload-text' and `0x0-upload-kill-ring', which
;; respectively upload (parts of) the current buffer, a file on your disk and a
;; string from the minibuffer can be used too.
;;
;; The default host and the host this package is named after is https://0x0.st.
;; Consider donating to https://liberapay.com/mia/donate if you like the service.


;; DumbParens
;;
;; Due to the currently ongoing issue of doing pairs propertly in smartparens this had to be made


; Set your pairs
(setq normal_chars   '("(" "[" "{" "\""))
(setq opposite_chars '(")" "]" "}" "\""))

; Inserts a matching character from the opposite_chars list
(defun insert-opposite-char (c)
  (unless (eq (length normal_chars) (length opposite_chars))
    (message "Your chars don't match")
    )
  (let ((len (length (member c normal_chars)))
        (orig_len (length opposite_chars)))
    (let ((oc (- orig_len len)))
      (insert (nth oc opposite_chars))
      )
    )
  )

;; Sees if there is a pair to complete
(defun after-insert ()
  (let ((c (char-to-string last-command-event)))
    (when (member c normal_chars)
      (unless (use-region-p)
        (insert-opposite-char c)
        (backward-char)
        )
      )
    )
  )

;; Surround
(defun after-selected-insert ()
  (interactive)
  (setq deactivate-mark nil)
  (let ((c (char-to-string last-command-event))
        (b (region-beginning))
        (e (region-end)))
    (progn
      (goto-char b)
      (insert c)
      (goto-char (1+ e))
      (insert-opposite-char c)
      )

    (set-mark b)
    (goto-char (+ e 2))
    (activate-mark)
    (setq deactivate-mark nil)
    )
  )

(defvar active-region-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  )

(define-minor-mode active-region-mode
  ""
  :init-value nil
  :lighter " Region"
  :keymap active-region-mode-map
  :group 'active-region
  )

;; Delete empty pairs of parens if any%
(defun on-delete (orig-func n &optional killflag)
  (if (<= (point) (buffer-size))
      (let* ((start (max (point-min) (- (point) n)))
             (str (buffer-substring-no-properties start (point)))
             (other (buffer-substring-no-properties (1+ start) (1+(point))))
             (rest1 (member str normal_chars))
             (rest2 (member other opposite_chars)))
        (when (not(null rest1))
          (when (= (length rest2) (length rest1))
            (delete-forward-char 1 (current-buffer))))))
  (funcall orig-func n killflag))

(defun active-region-on () (active-region-mode 1))
(defun active-region-off () (active-region-mode -1))

(defun refresh-shortcuts ()
  (dolist (n normal_chars)
    (define-key active-region-mode-map (kbd n) #'after-selected-insert)
    )
  )

(add-hook 'post-self-insert-hook 'after-insert)
(add-hook 'activate-mark-hook 'active-region-on)
(add-hook 'deactivate-mark-hook 'active-region-off)
(advice-add #'delete-backward-char :around #'on-delete)

(refresh-shortcuts)
