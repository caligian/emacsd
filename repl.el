;;; test.*el -*- lexical-binding: t; -*-
(provide 'repl)
(require 'vterm)
(require 'ht)

(setq repl-commands (ht ('ruby-mode "/usr/bin/irb --inf-ruby-mode")
			('hy-mode (nth 0 (whereis "hy")))
			('python-mode "/usr/bin/ipython")
			('lua-mode "/usr/bin/lua")
			('perl-mode "/usr/bin/perl")
			('raku-mode "/usr/bin/rakudo")
			('haskell-mode "/usr/bin/ghci")
			('sh-mode "/bin/bash")
			('scheme-mode "/usr/bin/guile")
			('r-mode "/usr/bin/R")
			('R-mode "/usr/bin/R")))

(setq compile-commands (ht ('ruby-mode "/usr/bin/ruby")
			   ('scheme-mode "/usr/bin/guile")
			   ('hy-mode (nth 0 (whereis "hy")))
			   ('python-mode "/usr/bin/ipython")
			   ('haskell-mode "/usr/bin/ghc -dynamic")
			   ('lua-mode "/usr/bin/lua")
			   ('perl-mode "/usr/bin/perl")
			   ('raku-mode "/usr/bin/rakudo")
			   ('sh-mode "/bin/bash")
			   ('r-mode "/usr/bin/R")
			   ('R-mode "/usr/bin/R")))

(setq build-commands (ht))

(defvar repl-running '())

(defvar repl-mode-map (make-sparse-keymap))

(defn repl-get (&optional (spec :name) (mode major-mode))
  (let* ((repl-cmd (ht-get repl-commands mode))
	 (compile-cmd (ht-get compile-commands mode))
	 (build-cmd (ht-get build-commands mode))
	 (bname (format "*%s-repl*" mode))
	 (proc (get-buffer-process bname)))
    (pcase spec
      (:process proc)
      (:name bname)
      (:buffer (get-buffer bname))
      (:repl repl-cmd)
      (:compile compile-cmd)
      (:build build-cmd))))

(defun repl-live? (&optional mode)
  (interactive)
  (process-live-p (repl-get :process mode)))

; You are responsible for putting the command correctly.
(defn repl-start-process (&optional (mode major-mode) (cmd (repl-get :repl mode)))
  (interactive)
  (let* ((name (repl-get :name mode))
         (continue? nil)
         (current (buffer-name))
         (proc (get-buffer-process name)))
    (unless cmd
      (error "No command defined for %s" mode))
    (unless proc
      (vterm name)
      (while (not continue?)
        (setf continue? (not (equal (buffer-name) current))))
      (vterm-send-string (concat cmd "\n"))
      (switch-to-buffer current)
      (cl-pushnew name repl-running))
    (get-buffer-process name)))

(defun repl-kill-process (&optional mode)
  (interactive)
  (let ((proc (repl-get :process mode))
        (buffer (repl-get :buffer mode)))
    (when proc
      (kill-process proc)
      (kill-buffer buffer)
      (setq repl-running (remove mode repl-running))
      t)))

(defn repl-assert-live (&optional (mode major-mode))
  (unless (repl-live? mode)
    (error "No process running for %s" mode)))

(defn repl-get-thing-at-point (&optional (what 'sexp))
  (if (and (featurep 'evil)
	   (not (or (eq 'insert evil-state)
		    (eq 'emacs evil-state))))
      (save-excursion
        (forward-char)
        (thing-at-point what t))
    (thing-at-point what t)))

(defn repl-send (s &optional (mode major-mode))
  (repl-assert-live mode)
  (let* ((s (s-chomp (cond
                      ((keywordp s)
		       (pcase s
                         (:region
                          (buffer-substring-no-properties (mark) (point)))
                         (:till-point
                          (buffer-substring-no-properties (point-min) (point)))
                         (:line
                          (repl-get-thing-at-point 'line))
                         (:buffer
                          (repl-get-thing-at-point 'buffer))
                         (_
                          (repl-get-thing-at-point 'line))))
                      ((symbolp s)
		       (repl-get-thing-at-point s))
                      (t s)))))
    (process-send-string (repl-get :process mode) (concat s "\n"))))

(defn repl-hide (&optional (mode major-mode))
  (interactive)
  (repl-assert-live mode)
  (let ((win (get-buffer-window (repl-get :buffer mode) 'visible)))
    (when win
      (delete-window win))))

(defn repl-show (&optional (split :s) (mode major-mode))
  (repl-assert-live mode)
  (let* ((buffer (repl-get :buffer mode))
         (win (get-buffer-window buffer 'visible)))
    (when win
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

(defun repl-show-split (&optional mode)
  (repl-show :s mode))

(defun repl-show-vsplit (&optional mode)
  (repl-show :v mode))

(defun repl-ivy--get-mode-from-name (bufname)
  (intern (replace-regexp-in-string
           "[*]\\([a-z-]+\\)-repl[*]"
           "\\1"
           bufname)))

(defun repl-ivy--split (bufname)
  (with-ivy-window
    (repl-show :s (repl-ivy--get-mode-from-name bufname))))

(defun repl-ivy--vsplit (bufname)
  (with-ivy-window
    (repl-show :v (repl-ivy--get-mode-from-name bufname))))

(defun repl-ivy--kill (bufname)
  (with-ivy-window
    (repl-kill-process (repl-ivy--get-mode-from-name bufname))))

(defun repl-ivy--send-string (bufname)
  (with-ivy-window
    (let* ((mode (repl-ivy--get-mode-from-name bufname))
           (prompt (format "(%s) > " mode))
           (userint (read-from-minibuffer prompt)))
      (when (> (length userint) 0)
        (repl-send (concat userint "\n") mode)))))

(defun repl-ivy--start (mode)
  (repl-start-process (intern mode)))

(defun repl-ivy--start-and-split (mode)
  (with-ivy-window
    (repl-ivy--start mode)
    (repl-ivy--split (format "*%s-repl*" mode))))

(defun repl-ivy--start-and-vsplit (mode)
  (with-ivy-window
    (repl-ivy--start mode)
    (repl-ivy--vsplit (format "*%s-repl*" mode))))

(defun repl-ivy-start nil
  (interactive)
  (ivy-read (format "Start REPL (%s) > " major-mode)
            (ht-keys repl-commands)
            :initial-input (symbol-name major-mode)
            :require-match t
            :action '(1
                      ("o" repl-ivy--start "Start for current buffer")
                      ("s" repl-ivy--start-and-split "Start and split")
                      ("v" repl-ivy--start-and-vsplit "Start and vsplit"))))

(defun repl-ivy-running nil
  (interactive)
  (ivy-read "Running REPLs > "
            (cl-remove-if-not #'get-buffer-process repl-running)
            :require-match t
            :action '(1
                      ("o" repl-ivy--split "Split")
                      ("s" repl-ivy--split "Split")
                      ("v" repl-ivy--vsplit "Vsplit")
                      ("k" repl-ivy--kill "Kill")
                      ("i" repl-ivy--send-string "Send string"))))

(defun repl-send-sexp nil
  (interactive)
  (repl-send 'sexp))

(defun repl-send-defun nil
  (interactive)
  (repl-send 'defun))

(defun repl-start nil
  (interactive)
  (repl-start-process))

(defun repl-kill nil
  (interactive)
  (repl-kill-process))

(defun repl-split nil
  (interactive)
  (repl-show :s))

(defun repl-vsplit nil
  (interactive)
  (repl-show :v))

(defun repl-send-region nil
  (interactive)
  (repl-send :region))

(defun repl-send-line nil
  (interactive)
  (repl-send :line))

(defun repl-send-buffer nil
  (interactive)
  (repl-send :buffer))

(defun repl-send-till-point nil
  (interactive)
  (repl-send :till-point))

(defun repl-send-eof nil
  (interactive)
  (repl-send 'eof))

(defun repl-send-string nil
  (interactive)
  (if (repl-live?)
      (let* ((prompt (format "(%s) > " major-mode))
             (s (read-from-minibuffer prompt)))
        (when (> (length s) 0)
          (repl-send s)))))

(defun repl-shell-start nil
  (interactive)
  (repl-start-process 'sh-mode))

(defun repl-shell-kill nil
  (interactive)
  (repl-kill-process 'sh-mode))

(defun repl-shell-hide nil
  (interactive)
  (repl-hide 'sh-mode))

(defun repl-shell-split nil
  (interactive)
  (repl-show :s 'sh-mode))

(defun repl-shell-vsplit nil
  (interactive)
  (repl-show :v 'sh-mode))

(defun repl-shell-send-region nil
  (interactive)
  (repl-send :region 'sh-mode))

(defun repl-shell-send-line nil
  (interactive)
  (repl-send :line 'sh-mode))

(defun repl-shell-send-buffer nil
  (interactive)
  (repl-send :buffer 'sh-mode))

(defun repl-shell-send-till-point nil
  (interactive)
  (repl-send :till-point 'sh-mode))

(defun repl-shell-send-eof nil
  (interactive)
  (repl-send 'eof 'sh-mode))

(defun repl-shell-send-string nil
  (interactive)
  (if (repl-live?)
      (let* ((prompt (format "(%s) > " major-mode))
             (s (read-from-minibuffer prompt)))
        (when (> (length s) 0)
          (repl-send s 'sh-mode)))))

(defun repl-shell-send-sexp nil
  (interactive)
  (repl-send 'sexp 'sh-mode))

(defun repl-shell-send-defun nil
  (interactive)
  (repl-send 'defun 'sh-mode))

; Additional utilities
(defn compile-buffer (&optional (mode major-mode) (cmd (ht-get compile-commands mode)))
  "Compile current buffer according to its MODE using CMD either user supplied or stored in `compile-commands'"
  (interactive)
  (unless cmd
    (error "No compile command provided for %s" mode))
  (compile (format "%s %s" cmd (current-buffer))))

(defn build-buffer (&optional (mode major-mode) (cmd (ht-get build-commands mode)))
  "Build current buffer according to its MODE using CMD either user supplied or stored in `build-commands'"
  (interactive)
  (unless cmd
    (error "No build command provided for %s" mode))
  (compile (format "%s %s" cmd (current-buffer))))

;; Keybindings
(alt-leader-r
  "R" #'repl-shell-send-region
  "&" #'repl-shell-start
  "q" #'repl-shell-kill
  "s" #'repl-shell-split
  "v" #'repl-shell-vsplit
  "k" #'repl-shell-hide
  "l" #'repl-shell-send-line
  "b" #'repl-shell-send-buffer
  "." #'repl-shell-send-till-point
  "i" #'repl-shell-send-string
  "c" #'repl-shell-send-eof
  "e" #'repl-shell-send-sexp
  "d" #'repl-shell-send-defun
  "r" #'repl-send-region  
  "!" #'repl-start
  "C-q" #'repl-kill
  "C-s" #'repl-split
  "C-v" #'repl-vsplit
  "C-k" #'repl-hide
  "C-l" #'repl-send-line
  "C-b" #'repl-send-buffer
  "C-." #'repl-send-till-point
  "C-i" #'repl-send-string
  "C-c" #'repl-send-eof
  "C-d" #'repl-send-defun
  "C-e" #'repl-send-sexp
  "C-," #'repl-ivy-running
  "C-`" #'repl-ivy-start)

(leader-r
  "R" #'repl-shell-send-region
  "&" #'repl-shell-start
  "q" #'repl-shell-kill
  "s" #'repl-shell-split
  "v" #'repl-shell-vsplit
  "k" #'repl-shell-hide
  "l" #'repl-shell-send-line
  "b" #'repl-shell-send-buffer
  "." #'repl-shell-send-till-point
  "i" #'repl-shell-send-string
  "c" #'repl-shell-send-eof
  "e" #'repl-shell-send-sexp
  "d" #'repl-shell-send-defun
  "r" #'repl-send-region  
  "!" #'repl-start
  "C-q" #'repl-kill
  "C-s" #'repl-split
  "C-v" #'repl-vsplit
  "C-k" #'repl-hide
  "C-l" #'repl-send-line
  "C-b" #'repl-send-buffer
  "C-." #'repl-send-till-point
  "C-i" #'repl-send-string
  "C-c" #'repl-send-eof
  "C-d" #'repl-send-defun
  "C-e" #'repl-send-sexp
  "C-," #'repl-ivy-running
  "C-`" #'repl-ivy-start)

(defkey :keymaps 'repl-mode-map
  "Q" #'repl-shell-kill
  "K" #'repl-shell-hide
  "E" #'repl-shell-send-sexp
  "L" #'repl-shell-send-line
  "B" #'repl-shell-send-buffer
  "D" #'repl-shell-send-defun
  ">" #'repl-shell-send-till-point
  "S" #'repl-shell-split
  "V" #'repl-shell-vsplit
  "R" #'repl-shell-send-region
  "r" #'repl-send-region
  "q" #'repl-kill
  "k" #'repl-hide
  "l" #'repl-send-line
  "e" #'repl-send-sexp
  "b" #'repl-send-buffer
  "d" #'repl-send-defun
  "." #'repl-send-till-point
  "s" #'repl-split
  "v" #'repl-vsplit)

(defkey
  "<f1>" #'repl-mode
  "<f2>" #'compile-buffer
  "<f3>" #'build-buffer)

(define-minor-mode repl-mode
  "Basic REPL extension for emacs"
  :init-value nil
  :lighter " repl"
  :keymap repl-mode-map
  (if (bound-and-true-p repl-mode)
      (if (repl-get :repl)
	  (repl-start)
	(progn
	  (message "No command defined for %s" major-mode)
	  (setq repl-mode nil)))))
