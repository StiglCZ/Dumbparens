;;; dumbparens.el --- Attempts to make paren pairs and tries to not be smart about it -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2024-2024 Stigl
;;
;; Author: Stigl <stigl@ireview.games>
;; Maintainer: Stigl <stigl@ireview.games>
;; Created: December 25, 2024
;; Modified: December 27, 2024
;; Version: 1.0.0
;; Homepage: https://github.com/StiglCZ/Dumbparens
;; Package-Requires: ((emacs "26.4"))
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
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

(setq dp_normal_chars   '("(" "[" "{" "\""))
(setq dp_opposite_chars '(")" "]" "}" "\""))

; Inserts a matching character from the opposite_chars list
(defun dp-insert-opposite-char (c)
  (unless (eq (length dp_normal_chars) (length dp_opposite_chars))
    (message "Your chars don't match")
    )
  (let ((len (length (member c dp_normal_chars)))
        (orig_len (length dp_opposite_chars)))
    (let ((oc (- orig_len len)))
      (insert (nth oc dp_opposite_chars))
      )
    )
  )

;; Sees if there is a pair to complete
(defun dp-after-insert ()
  (let ((c (char-to-string last-command-event)))
    (when (member c dp_normal_chars)
      (unless (use-region-p)
        (dp-insert-opposite-char c)
        (backward-char)
        )
      )
    )
  )

;; Surround
(defun dp-after-selected-insert ()
  (interactive)
  (setq deactivate-mark nil)
  (let ((c (char-to-string last-command-event))
        (b (region-beginning))
        (e (region-end)))
    (progn
      (goto-char b)
      (insert c)
      (goto-char (1+ e))
      (dp-insert-opposite-char c)
      )

    (set-mark b)
    (goto-char (+ e 2))
    (activate-mark)
    (setq deactivate-mark nil)
    )
  )

(defvar dp-active-region-mode-map
  (let ((map (make-sparse-keymap)))
    map)
  )

(define-minor-mode dp-active-region-mode
  ""
  :init-value nil
  :lighter " Region"
  :keymap dp-active-region-mode-map
  :group 'dp-active-region
  )

;; Delete empty pairs of parens if any%
(defun dp-on-delete (orig-func n &optional killflag)
  (if (<= (point) (buffer-size))
      (let* ((start (max (point-min) (- (point) n)))
             (str (buffer-substring-no-properties start (point)))
             (other (buffer-substring-no-properties (1+ start) (1+(point))))
             (rest1 (member str dp_normal_chars))
             (rest2 (member other dp_opposite_chars)))
        (when (not(null rest1))
          (when (= (length rest2) (length rest1))
            (delete-forward-char 1 (current-buffer))))))
  (funcall orig-func n killflag))

(defun dp-active-region-on () (dp-active-region-mode 1))
(defun dp-active-region-off () (dp-active-region-mode -1))

;;;###autoload
(defun dp-refresh-shortcuts ()
  (interactive)
  (dolist (n dp_normal_chars)
    (define-key dp-active-region-mode-map (kbd n) #'dp-after-selected-insert)
    )
  )

;;;###autoload
(defun dumbparens ()
  (interactive)
  (dp-refresh-shortcuts)
  )

(add-hook 'post-self-insert-hook 'dp-after-insert)
(add-hook 'activate-mark-hook 'dp-active-region-on)
(add-hook 'deactivate-mark-hook 'dp-active-region-off)
(advice-add #'delete-backward-char :around #'dp-on-delete)

;;; dumbparens.el ends here
