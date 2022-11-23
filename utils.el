(defun add-hooks (&rest hooks)
  "Add multiple hooks conveniently

Each FORM should be a quoted list with car as HOOK and cdr as FUNC1, FUNC2, ...
FUNCN can either be a function symbol or a list where car as the function and cdr as plist containing the two optional keywords :local and :depth as used in `add-hook'

Example
(add-hooks '(python-mode-hook treesitter-mode electric-indent-mode elpy) '(ruby-mode-hook treesitter-mode electric-indent-mode elpy))

(add-hooks '(python-mode-hook (treesitter-mode :local t) (electric-indent-mode :local t)))
"
  (dolist (x hooks)
    (if-let* ((hook (car x))
	      (funcs (cdr x)))
	(if (listp funcs)
	    (dolist (f funcs)
	      (if (listp f)
                  (let* ((-f (car f))
			 (f (cdr f))
			 (local (plist-get f :local))
	 		 (depth (plist-get f :depth)))
		    (add-hook hook -f depth local))
		(add-hook hook f)))
	  (add-hook hook f))
      (error "No functions provided for hook %s" hook))))

(defun trim-string (s)
  "Trim string on left and right ends

Removes tab, whitespace and/or newline from left and right ends of a string"
  (replace-regexp-in-string "\\(^[
	 ]*\\|[
	 ]*$\\)" "" s))

(defun whereis (prog)
  "whereis equivalent in elisp

Returns a list of executable paths found or nil otherwise"
  (if-let* ((cmd (format "whereis %s | cut -d : -f 2-" prog))
	    (out (trim-string (shell-command-to-string cmd)))
	    (out (if (= 0 (length out))
		     nil
		   out)))
      (split-string out " ")))
