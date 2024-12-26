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

(defun refresh-shortcuts ()
  (dolist (n normal_chars)
    (define-key active-region-mode-map (kbd n) #'after-selected-insert)
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

;; Delete empty pairs of parens if any%
(defun on-delete (orig-func n &optional killflag)
  (if (<= (point) (buffer-size))
      (let* ((start (max (point-min) (- (point) n)))
             (str (buffer-substring-no-properties start (point)))
             (other (buffer-substring-no-properties (1+ start) (1+(point)))))
        (when (member str normal_chars)
          (when (member other opposite_chars)
            (delete-forward-char 1 (current-buffer))))))
  (funcall orig-func n killflag))

(defun active-region-on () (active-region-mode 1))
(defun active-region-off () (active-region-mode -1))

(add-hook 'post-self-insert-hook 'after-insert)
(add-hook 'activate-mark-hook 'active-region-on)
(add-hook 'deactivate-mark-hook 'active-region-off)
(advice-add #'delete-backward-char :around #'on-delete)
(refresh-shortcuts)
