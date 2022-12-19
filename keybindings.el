(require 'general)

;; Syntax:
;; (FORMS...)
;; FORM is (<prefix> <states> ...) where ... consists of rest args passed to general-define-key
(leader-bind!
 ;; Search
 (/
  normal
  "/" rg
  "o" occur
  "p" projectile-rg)

 ;; File management
 (f
  normal
  "f" find-file
  "C-f" find-file-read-only
  "s" save-buffer
  "r" counsel-recentf)

 ;; Buffer management
 (b
  normal
  "b" switch-to-buffer
  "i" ibuffer
  "q" kill-buffer
  "k" delete-window)

 ;; Window management
 (w
  normal
  "o" "C-x 1"
  "q" "C-x 0"
  "k" windmove-up
  "j" windmove-down
  "h" windmove-left
  "l" windmove-right
  "s" split-window-below
  "v" split-window-right)

 ;; Help stuff
 (h
  normal
  "t" counsel-load-theme
  "s" company-show-doc-buffer)

 ;; Treemacs
 (single-quote
  normal
  "" treemacs)

 ;; Magit
 (g
  normal
  "g" magit-status
  "s" magit-stage-file
  "c" magit-commit
  "p" magit-push
  "C-p" magit-pull
  "b" magit-branch
  "i" magit-init
  "u" magit-unstage)

 ;; Projectile
 (p
  normal
  "." projectile-commander
  "p" projectile-switch-project
  "a" projectile-add-known-project
  "d" projectile-find-dir
  "C-d" projectile-dired
  "f" projectile-find-file)

 ;; Emacs eval stuff
 (e
  (normal visual)
  :keymaps emacs-lisp-mode-map
  "e" eval-last-sexp
  "b" eval-buffer
  "r" eval-region
  "d" eval-defun
  ":" eval-expression)

 ;; Company
 (tab
  normal
  "" company-complete))

(leader-bind `(q
	       normal
	       "q"
	       ,(general-simulate-key "C-x C-k" :state 'emacs)))

;; Alternative window bindings
(defkey :prefix "C-x w"
  "o" "C-x 1"
  "q" "C-x 0"
  "k" #'windmove-up
  "j" #'windmove-down
  "h" #'windmove-left
  "l" #'windmove-right
  "s" #'split-window-below
  "v" #'split-window-right)

;; Align regions 
(defkey
  "C-=" #'align
  "C-+" #'align-regexp)

;; Smartparens
(states-bind!
 ((n v o)
  :keymaps smartparens-mode-map
  "w" sp-forward-sexp
  "W" sp-next-sexp
  "b" sp-backward-sexp
  "B" sp-previous-sexp))
