(require 'general)

(setq user-leader-translations '((single-quote . "'")
				 (double-quote . "\"")
				 (tab . "<tab>")
				 (spc . "SPC")
				 (semicolon . ";")))

(defmacro create-leader-defun (prefix)
  (let* ((tr (or (assoc prefix user-leader-translations) prefix))
	 (tr (if (listp tr) (cdr tr) tr))
	 (tr (if (symbolp tr) (symbol-name tr) tr))
	 (prefix (symbol-name prefix))
	 (name (format "leader-%s" prefix))
	 (name (intern name))
	 (keys-ll (concat "M-SPC " tr))
	 (keys-l (concat "SPC " tr))
	 (prefix-ll `(:prefix ,keys-ll))
	 (prefix-l `(:prefix ,keys-l)))
    `(defun ,name (states &rest args)
       (let* ((states (list :states states)))
	 (apply 'defkey (append ',prefix-l states args))
	 (apply 'defkey (append ',prefix-ll args))))))

(defun user-create-leader-keybinding (form)
  (let* ((prefix (nth 0 form))
	 (states (nth 1 form))
	 (args (cdr (cdr form)))
	 (definer (intern (format "leader-%s" prefix))))
    (print definer)
    (apply definer (list states) args)))

(defun user-create-leader-keybindings (mappings)
  (mapcar 'user-create-leader-keybinding form))

(defmacro leader-bind* (&rest mappings)
  `(mapcar 'user-create-leader-keybinding ',mappings))

(create-leader-defun z)
(create-leader-defun y)
(create-leader-defun x)
(create-leader-defun w)
(create-leader-defun v)
(create-leader-defun u)
(create-leader-defun t)
(create-leader-defun s)
(create-leader-defun r)
(create-leader-defun q)
(create-leader-defun p)
(create-leader-defun o)
(create-leader-defun n)
(create-leader-defun m)
(create-leader-defun l)
(create-leader-defun k)
(create-leader-defun j)
(create-leader-defun i)
(create-leader-defun h)
(create-leader-defun g)
(create-leader-defun f)
(create-leader-defun e)
(create-leader-defun d)
(create-leader-defun c)
(create-leader-defun b)
(create-leader-defun a)
(create-leader-defun semicolon)
(create-leader-defun spc)
(create-leader-defun tab)
(create-leader-defun double-quote)
(create-leader-defun single-quote)
(create-leader-defun /)
(create-leader-defun *)