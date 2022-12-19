(require 'general)

(setq user-leader-translations '((single-quote . "'")
				 (double-quote . "\"")
				 (tab . "<tab>")
				 (spc . "SPC")
				 (semicolon . ";")))

(defmacro make-leader-defun (prefix)
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

(defun leader-bind (form)
  (let* ((prefix (nth 0 form))
	 (states (nth 1 form))
	 (args (cdr (cdr form)))
	 (definer (intern (format "leader-%s" prefix))))
    (apply definer `(,(ensure-list states) ,@args))))

(defun leader-bind* (mappings)
  (dolist (m mappings)
    (apply #'leader-bind `(,m))))

(defmacro leader-bind! (&rest mappings)
  `(apply 'leader-bind* (list ',mappings)))

(defun states-bind (form)
  (let* ((states (nth 0 form))
	 (args (cdr form)))
    (apply #'defkey :states (ensure-list states) args)))

(defun states-bind* (&rest forms)
  (dolist (f forms)
    (apply #'states-bind `(,f))))

(defmacro states-bind! (&rest forms)
  `(apply 'states-bind* ',forms))

(make-leader-defun z)
(make-leader-defun y)
(make-leader-defun x)
(make-leader-defun w)
(make-leader-defun v)
(make-leader-defun u)
(make-leader-defun t)
(make-leader-defun s)
(make-leader-defun r)
(make-leader-defun q)
(make-leader-defun p)
(make-leader-defun o)
(make-leader-defun n)
(make-leader-defun m)
(make-leader-defun l)
(make-leader-defun k)
(make-leader-defun j)
(make-leader-defun i)
(make-leader-defun h)
(make-leader-defun g)
(make-leader-defun f)
(make-leader-defun e)
(make-leader-defun d)
(make-leader-defun c)
(make-leader-defun b)
(make-leader-defun a)
(make-leader-defun semicolon)
(make-leader-defun spc)
(make-leader-defun tab)
(make-leader-defun double-quote)
(make-leader-defun single-quote)
(make-leader-defun /)
(make-leader-defun *)
