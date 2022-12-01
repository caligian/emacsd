(setq occur-defun (ht ('python-mode "^\s-*def\s-*[a-zA-Z_][a-zA-Z0-9_]")
		      ('ruby-mode "^def\s-*[a-zA-Z_][a-zA-Z0-9_?]")
		      ('lua-mode "^\s-*\\(local\s-*\\)?function")
		      ('emacs-lisp-mode "^(\\(defun\\|defn\\|cl-defun\\)")
		      ('scheme-mode "^(\\(define\\|lambda\\)\s-*([^)]+")
		      ('common-lisp-mode "^(defun")
		      ('sh-mode "^[a-zA-Z0-9]+\s-*()\s-*{")))
    
(setq occur-setq (ht))

(defn occur-show (&optional (what :defun) (mode major-mode))
  (if-let* ((mode (or mode major-mode))
	    (occur-t (pcase what
		       (:defun occur-defun)
		       (:setq occur-setq)
		       (_ :defun)))
	    (regex (ht-get occur-t mode)))
      (occur regex)
    (message "No %s definition regex defined for %s" what major-mode)))

(defun occur-show-defun (&optional mode)
  (interactive)
  (occur-show :defun mode))

(defun occur-show-setq (&optional mode)
  (interactive)
  (occur-show :setq mode))

;; Keybindings
(leader-/
  "d" 'occur-show-defun)

(alt-leader-/
  "d" 'occur-show-defun)
