;;; test.*el -*- lexical-binding: t; -*-
(provide 'repl)
(require 'general)
(require 's)
(require 'vterm)

(defun repl--get-executable (prog)
  (if-let* ((cmd (format "whereis %s | cut -d : -f 2 | awk '{print $1}'" prog))
            (out (s-chomp (shell-command-to-string cmd)))
            (out (if (not (= (length out) 0))
                     out
                   nil)))
    out))

(setq repl-commands `((ruby-mode . "/usr/bin/irb --inf-ruby-mode")
                      (hy-mode . ,(repl--get-executable "hy"))
                      (julia-mode . "julia -q")
                      (racket-mode . "/usr/bin/racket")
                      (perl-mode . "/usr/bin/perl")
                      (raku-mode . "/usr/bin/rakudo")
                      (python-mode . "/usr/bin/ipython3")
                      (lua-mode . "/usr/bin/lua")
                      (sh-mode . "/bin/bash")))

(defvar repl-running '())

(defvar repl-shell-mode-map (let ((map (make-sparse-keymap)))
                              (general-define-key :keymaps 'repl-shell-mode-map
                                                  :states 'normal
                                                  :prefix "g"
                                                  "Q" #'repl/shell-kill
                                                  "K" #'repl/shell-hide
                                                  "E" #'repl/shell-send-sexp
                                                  "L" #'repl/shell-send-line
                                                  "B" #'repl/shell-send-buffer
                                                  "D" #'repl/shell-send-defun
                                                  ">" #'repl/shell-send-till-point
                                                  "S" #'repl/shell-split
                                                  "V" #'repl/shell-vsplit)

                              (general-define-key
                               :keymaps 'repl-shell-mode-map
                               :states 'visual
                               :prefix "g"
                               "R" #'repl/shell-send-region)
                              map))

(defvar repl-mode-map (let ((map (make-sparse-keymap)))
                        (general-define-key :keymaps 'repl-mode-map
                                            :states 'normal
                                            :prefix "g"
                                            "q" #'repl/kill
                                            "k" #'repl/hide
                                            "l" #'repl/send-line
                                            "e" #'repl/send-sexp
                                            "b" #'repl/send-buffer
                                            "d" #'repl/send-defun
                                            "." #'repl/send-till-point
                                            "s" #'repl/split
                                            "v" #'repl/vsplit)

                        (general-define-key :keymaps 'repl-mode-map
                                            :states 'visual
                                            :prefix "g"
                                            "r" #'repl/send-region)
                        map))

(defun repl--get (spec &optional mode cmd)
  (let* ((spec (cond
                ((keywordp spec)
                 spec)
                ((not spec)
                 :buffer-name)
                (t t)))
         (mode (if (or (not mode)
                       (eq (intern "") mode))
                   major-mode
                 mode))
         (cmd (if (or (not cmd)
                      (string-empty-p cmd))
                  (cdr (assoc mode repl-commands))
                cmd))
         (name (format "*%s-repl*" mode))
         (buffer (get-buffer name))
         (proc (get-buffer-process name)))
    (pcase spec
      (:buffer-name name)
      (:buffer buffer)
      (:process proc)
      (:cmd cmd)
      (:mode mode)
      (_ `((:buffer-name ,name)
           (:buffer ,buffer)
           (:process ,proc)
           (:mode ,mode)
           (:cmd ,cmd))))))

(defun repl--live? (&optional mode)
  (process-live-p (repl--get :process mode)))

; You are responsible for putting the command correctly.
(defn repl--start-process (&optional (mode major-mode) (cmd (cdr (assoc mode repl-commands))))
  (let* ((cmd (or cmd (assoc mode repl-commands)))
         (name (format "*%s-repl*" mode))
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

;; (defun repl--start-process (&optional mode cmd)
;;   (let* ((mode (or mode major-mode))
;;          (cmd (ensure-list (or cmd (repl--get :cmd mode))))
;;          (args (cdr cmd))
;;          (cmd (car cmd))
;;          (name (buffer-name (get-buffer-create (repl--get :buffer-name mode)))))
;;     (unless cmd
;;       (error "No command defined for %s." mode))
;;     (when (or (not (get-process name))
;;               (not (process-live-p (get-process name))))
;;       (apply #'make-comint-in-buffer `(,name ,name ,cmd nil ,@args))
;;       (unless (get-process name)
;;         (kill-buffer name)
;;         (error "Could not start REPL for %s with command %s" mode cmd)))
;;     (add-to-list 'repl-running name)
;;     (get-process name)))

(defun repl--kill-process (&optional mode)
  (let ((proc (repl--get :process mode))
        (buffer (repl--get :buffer mode)))
    (when proc
      (kill-process proc)
      (kill-buffer buffer)
      (setq repl-running (remove mode repl-running))
      t)))

(defn repl--assert-live-process (&optional (mode major-mode))
  (unless (repl--live? mode)
    (error "No process running for %s" mode)))

(defn repl--get-thing-at-point (&optional (what 'sexp))
  (if (not (or (eq 'insert evil-state)
               (eq 'emacs evil-state)))
      (save-excursion
        (forward-char)
        (thing-at-point what t))
    (thing-at-point what t)))

(defn repl--send (s &optional (mode major-mode))
  (repl--assert-live-process mode)
  (let* ((s (s-chomp (cond
                      ((keywordp s) (cond
                                     ((eq s :region)
                                      (buffer-substring-no-properties (mark) (point)))
                                     ((eq s :till-point)
                                      (buffer-substring-no-properties (point-min) (point)))
                                     ((eq s :line)
                                      (repl--get-thing-at-point 'line))
                                     ((eq s :buffer)
                                      (repl--get-thing-at-point 'buffer))
                                     (t
                                      (repl--get-thing-at-point 'line))))
                      ((symbolp s) (repl--get-thing-at-point s))
                      (t s)))))
    (process-send-string (repl--get :process mode) (concat s "\n"))))

(defun repl--hide (&optional mode)
  (repl--assert-live-process mode)
  (let ((win (get-buffer-window (repl--get :buffer mode) 'visible)))
    (when win
      (delete-window win))))

(defun repl--show (&optional split mode)
  (repl--assert-live-process mode)
  (let* ((buffer (repl--get :buffer mode))
         (win (get-buffer-window buffer 'visible)))
    (when win
      (delete-window win))
    (cond ((eq split :s) (progn
                           (split-window-vertically)
                           (windmove-down)
                           (switch-to-buffer buffer)))
          ((eq split :v) (progn
                           (split-window-horizontally)
                           (windmove-right)
                           (switch-to-buffer buffer))))))

(defun repl--show-split (&optional mode)
  (repl--show :s mode))

(defun repl--show-vsplit (&optional mode)
  (repl--show :v mode))

(defun repl--parse-mode (&optional mode command)
  (let* ((mode (if (or (not mode)
                       (eql mode (intern "")))
                   major-mode
                 mode))
         (command (if (string-empty-p command)
                      nil
                    command)))
    `(,mode ,command)))

(defun repl-ivy--get-mode-from-name (bufname)
  (intern (replace-regexp-in-string
           "[*]\\([a-z-]+\\)-repl[*]"
           "\\1"
           bufname)))

(defun repl-ivy--split (bufname)
  (with-ivy-window
    (repl--show :s (repl-ivy--get-mode-from-name bufname))))

(defun repl-ivy--vsplit (bufname)
  (with-ivy-window
    (repl--show :v (repl-ivy--get-mode-from-name bufname))))

(defun repl-ivy--kill (bufname)
  (with-ivy-window
    (repl--kill-process (repl-ivy--get-mode-from-name bufname))))

(defun repl-ivy--send-string (bufname)
  (with-ivy-window
    (let* ((mode (repl-ivy--get-mode-from-name bufname))
           (prompt (format "(%s) > " mode))
           (userint (read-from-minibuffer prompt)))
      (when (> (length userint) 0)
        (repl--send (concat userint "\n") mode)))))

(defun repl-ivy--start (mode)
  (repl--start-process (intern mode)))

(defun repl-ivy--edit (mode &optional run)
  (let* ((mode (intern mode))
         (command (nthcdr 1 (assoc mode repl-commands)))
         (prompt (format "%s -> ? > " command))
         (new (read-from-minibuffer prompt)))
    (if (> (length new) 0)
        (setcdr (assoc mode repl-commands) new)
      (message "No command provided fou %s.\nUsing default command: %s" mode command))
    (if run
        (repl--start-process mode))))

(defun repl-ivy--edit-and-run (mode)
  (repl-ivy--edit mode t))

(defun repl-ivy--start-and-split (mode)
  (with-ivy-window
    (repl-ivy--start mode)
    (repl-ivy--split (format "*%s-repl*" mode))))

(defun repl-ivy--start-and-vsplit (mode)
  (with-ivy-window
    (repl-ivy--start mode)
    (repl-ivy--vsplit (format "*%s-repl*" mode))))

(defun repl-ivy/start nil
  (interactive)
  (ivy-read (format "Start REPL (%s) > " major-mode)
            (mapcar #'car repl-commands)
            :initial-input (symbol-name major-mode)
            :require-match t
            :action '(1
                      ("o" repl-ivy--start "Start for current buffer")
                      ("e" repl-ivy--edit  "Edit command")
                      ("E" repl-ivy--edit-and-run "Edit command and start")
                      ("s" repl-ivy--start-and-split "Start and split")
                      ("v" repl-ivy--start-and-vsplit "Start and vsplit"))))

(defun repl-ivy/running nil
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

(defun repl/send-sexp nil
  (interactive)
  (repl--send 'sexp))

(defun repl/send-defun nil
  (interactive)
  (repl--send 'defun))

(defun repl/start nil
  (interactive)
  (repl--start-process))

(defun repl/kill nil
  (interactive)
  (repl--kill-process))

(defun repl/hide nil
  (interactive)
  (repl--hide))

(defun repl/split nil
  (interactive)
  (repl--show :s))

(defun repl/vsplit nil
  (interactive)
  (repl--show :v))

(defun repl/send-region nil
  (interactive)
  (repl--send :region))

(defun repl/send-line nil
  (interactive)
  (repl--send :line))

(defun repl/send-buffer nil
  (interactive)
  (repl--send :buffer))

(defun repl/send-till-point nil
  (interactive)
  (repl--send :till-point))

(defun repl/send-eof nil
  (interactive)
  (repl--send 'eof))

(defun repl/send-string nil
  (interactive)
  (if (repl--live?)
      (let* ((prompt (format "(%s) > " major-mode))
             (s (read-from-minibuffer prompt)))
        (when (> (length s) 0)
          (repl--send s)))))

(defun repl/shell-start nil
  (interactive)
  (repl--start-process 'sh-mode))

(defun repl/shell-kill nil
  (interactive)
  (repl--kill-process 'sh-mode))

(defun repl/shell-hide nil
  (interactive)
  (repl--hide 'sh-mode))

(defun repl/shell-split nil
  (interactive)
  (repl--show :s 'sh-mode))

(defun repl/shell-vsplit nil
  (interactive)
  (repl--show :v 'sh-mode))

(defun repl/shell-send-region nil
  (interactive)
  (repl--send :region 'sh-mode))

(defun repl/shell-send-line nil
  (interactive)
  (repl--send :line 'sh-mode))

(defun repl/shell-send-buffer nil
  (interactive)
  (repl--send :buffer 'sh-mode))

(defun repl/shell-send-till-point nil
  (interactive)
  (repl--send :till-point 'sh-mode))

(defun repl/shell-send-eof nil
  (interactive)
  (repl--send 'eof 'sh-mode))

(defun repl/shell-send-string nil
  (interactive)
  (if (repl--live?)
      (let* ((prompt (format "(%s) > " major-mode))
             (s (read-from-minibuffer prompt)))
        (when (> (length s) 0)
          (repl--send s 'sh-mode)))))

(defun repl/shell-send-sexp nil
  (interactive)
  (repl--send 'sexp 'sh-mode))

(defun repl/shell-send-defun nil
  (interactive)
  (repl--send 'defun 'sh-mode))

(define-minor-mode repl-mode
  "Basic REPL extension for emacs"
  :lighter " repl"
  :keymap repl-mode-map
  (let ((cmd (repl--get :cmd)))
    (cond
     ((and cmd repl-mode)
      (repl/start))
     ((not cmd)
      (message "No command specified for %s" major-mode)))))

(define-minor-mode repl-shell-mode
  "Basic REPL extension for emacs"
  :lighter " repl-shell"
  :keymap repl-shell-mode-map
  (if repl-shell-mode
      (repl/shell-start)))

(general-define-key :states 'visual
                      :prefix "SPC r"
                      :non-normal-prefix "M-SPC r"
                      "R" #'repl/shell-send-region
                      "r" #'repl/send-region)

(general-define-key :states 'normal
                    :prefix "SPC r"
                    :non-normal-prefix "M-SPC r"
                    "&" #'repl/shell-start
                    "Q" #'repl/shell-kill
                    "S" #'repl/shell-split
                    "V" #'repl/shell-vsplit
                    "K" #'repl/shell-hide
                    "L" #'repl/shell-send-line
                    "B" #'repl/shell-send-buffer
                    ">" #'repl/shell-send-till-point
                    "I" #'repl/shell-send-string
                    "C" #'repl/shell-send-eof
                    "E" #'repl/shell-send-sexp
                    "D" #'repl/shell-send-defun
                    "!" #'repl/start
                    "q" #'repl/kill
                    "s" #'repl/split
                    "v" #'repl/vsplit
                    "k" #'repl/hide
                    "l" #'repl/send-line
                    "b" #'repl/send-buffer
                    "." #'repl/send-till-point
                    "i" #'repl/send-string
                    "c" #'repl/send-eof
                    "d" #'repl/send-defun
                    "e" #'repl/send-sexp
                    "," #'repl-ivy/running
                    "`" #'repl-ivy/start)

(global-set-key (kbd "<f1>") #'repl-mode)
(global-set-key (kbd "<f2>") #'repl-shell-mode)

(setq compile-commands `((ruby-mode . "/usr/bin/ruby")
                         (hy-mode . ,(repl--get-executable "hy"))
                         (julia-mode . "/usr/bin/julia")
                         (racket-mode . "/usr/bin/racket")
                         (python-mode . "/usr/bin/python")
                         (lua-mode . "/usr/bin/lua")
                         (sh-mode . "/bin/bash")))


; Additional utilities
(defn compile-buffer (&optional (mode major-mode) (cmd (cdr (assoc mode compile-commands))))
  "Compile current buffer according to its MODE using CMD either user supplied or stored in `compile-command'"
  (interactive)
  (unless cmd
    (error "No compile command provided for %s" mode))
  (compile (format "%s %s" cmd (current-buffer))))

(map! :leader "cc" 'compile-buffer)
