(defun trim-string (s)
  "Trim string on left and right ends

Removes tab, whitespace and/or newline from left and right ends of a string"
  (replace-regexp-in-string "\\(^[
	 ]*\\|[
	 ]*$\\)" "" s))

(defun whereis (prog &optional default)
  "whereis equivalent in elisp

Returns a list of executable paths found or nil otherwise"
  (if-let* ((cmd (format "whereis %s | cut -d : -f 2-" prog))
	    (out (trim-string (shell-command-to-string cmd)))
	    (out (if (= 0 (length out))
		     nil
		   out)))
      (split-string out " ")
    (list default)))

(defn get-buffer-by-regexp (&optional (regexp ".") (frame 'visible))
  "Get buffers with regular expression

Returns a list of buffer objects matched by REGEXP or nil
"
  (let* ((buffers (cl-loop for buffer in (buffer-list frame)
			   collect (buffer-name buffer)))
	 (found nil))
    (dolist (b buffers)
      (when (string-match-p regexp b)
	(push (get-buffer b) found)))
    found))

(defn split-window-and-switch-to-buffer (&optional (split :s) (buffer (current-buffer)))
  "Split window horizontally or vertically"
  (let* ((buffer (get-buffer-create buffer))
	 (win (get-buffer-window buffer t))
	 (all-win (window-list))
	 (more-than-one? (> (length all-win) 1))
	 (no-same-buffer (not (eq (current-buffer) buffer))))
    (when (and more-than-one? win)
      (delete-window win))
    (pcase split
      (:s (progn
            (split-window-vertically)
            (windmove-down)
            (switch-to-buffer buffer)))
      (:v (progn
            (split-window-horizontally)
            (windmove-right)
            (switch-to-buffer buffer))))))

(defn get-shell-path (&optional (path "~/.bashrc"))
  (if-let* ((envpath (with-temp-buffer
		       (insert-file-contents path)
		       (buffer-string)))
	    (envpath (save-match-data
		       (string-match "PATH=\"\\([^\"]+\\)" envpath)
		       (match-string 1 envpath)))
	    (envpath (replace-regexp-in-string "\\$HOME" (getenv "HOME") envpath t))
	    (envpath (split-string envpath ":"))
	    (envpath (catch 'break
		       (dolist (i (number-sequence 0 (- (length envpath) 1)))
			 (let ((p (nth i envpath)))
			   (when (equal p "$PATH")
			     (setf (nth i envpath) nil)
			     (throw 'break envpath)))))))
      (cl-loop for x in envpath
	       when x
	       collect x)))

(defun append-exec-path-from-shell (&optional path)
  (if-let* ((envpath (get-shell-path path)))
      (dolist (x envpath)
	(push x exec-path))))
