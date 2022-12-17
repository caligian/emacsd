(require 'general)

(global-unset-key (kbd "M-SPC"))

(general-create-definer alt-leader-quote :prefix "M-SPC '")
(general-create-definer alt-leader-tab :prefix "M-SPC <tab>")
(general-create-definer alt-leader-a :prefix "M-SPC a")
(general-create-definer alt-leader-b :prefix "M-SPC b")
(general-create-definer alt-leader-c :prefix "M-SPC c")
(general-create-definer alt-leader-d :prefix "M-SPC d")
(general-create-definer alt-leader-e :prefix "M-SPC e")
(general-create-definer alt-leader-f :prefix "M-SPC f")
(general-create-definer alt-leader-g :prefix "M-SPC g")
(general-create-definer alt-leader-h :prefix "M-SPC h")
(general-create-definer alt-leader-i :prefix "M-SPC i")
(general-create-definer alt-leader-j :prefix "M-SPC j")
(general-create-definer alt-leader-k :prefix "M-SPC k")
(general-create-definer alt-leader-l :prefix "M-SPC l")
(general-create-definer alt-leader-m :prefix "M-SPC m")
(general-create-definer alt-leader-n :prefix "M-SPC n")
(general-create-definer alt-leader-o :prefix "M-SPC o")
(general-create-definer alt-leader-p :prefix "M-SPC p")
(general-create-definer alt-leader-q :prefix "M-SPC q")
(general-create-definer alt-leader-r :prefix "M-SPC r")
(general-create-definer alt-leader-s :prefix "M-SPC s")
(general-create-definer alt-leader-t :prefix "M-SPC t")
(general-create-definer alt-leader-u :prefix "M-SPC u")
(general-create-definer alt-leader-v :prefix "M-SPC v")
(general-create-definer alt-leader-w :prefix "M-SPC w")
(general-create-definer alt-leader-x :prefix "M-SPC x")
(general-create-definer alt-leader-y :prefix "M-SPC y")
(general-create-definer alt-leader-z :prefix "M-SPC z")
(general-create-definer alt-leader-/ :prefix "M-SPC /")

;; default leader
(general-create-definer leader-quote :prefix "SPC '")
(general-create-definer leader-tab :prefix "SPC <tab>")
(general-create-definer leader-a :prefix "SPC a")
(general-create-definer leader-b :prefix "SPC b")
(general-create-definer leader-c :prefix "SPC c")
(general-create-definer leader-d :prefix "SPC d")
(general-create-definer leader-e :prefix "SPC e")
(general-create-definer leader-f :prefix "SPC f")
(general-create-definer leader-g :prefix "SPC g")
(general-create-definer leader-h :prefix "SPC h")
(general-create-definer leader-i :prefix "SPC i")
(general-create-definer leader-j :prefix "SPC j")
(general-create-definer leader-k :prefix "SPC k")
(general-create-definer leader-l :prefix "SPC l")
(general-create-definer leader-m :prefix "SPC m")
(general-create-definer leader-n :prefix "SPC n")
(general-create-definer leader-o :prefix "SPC o")
(general-create-definer leader-p :prefix "SPC p")
(general-create-definer leader-q :prefix "SPC q")
(general-create-definer leader-r :prefix "SPC r")
(general-create-definer leader-s :prefix "SPC s")
(general-create-definer leader-t :prefix "SPC t")
(general-create-definer leader-u :prefix "SPC u")
(general-create-definer leader-v :prefix "SPC v")
(general-create-definer leader-w :prefix "SPC w")
(general-create-definer leader-x :prefix "SPC x")
(general-create-definer leader-y :prefix "SPC y")
(general-create-definer leader-z :prefix "SPC z")
(general-create-definer leader-/ :prefix "SPC /")

;; Misc
(evil-map :nv ":" 'execute-extended-command)

;; Search
(leader-/
  :states 'normal
  "/" 'rg
  "o" 'occur
  "p" 'projectile-rg)

(alt-leader-/
  :states 'normal
  "/" 'rg
  "o" 'occur
  "p" 'projectile-rg)

;; File management
(leader-f
  :states 'normal
  "f" #'find-file
  "C-f" #'find-file-read-only
  "s" #'save-buffer
  "r" #'counsel-recentf)

(alt-leader-f
  :states 'normal
  "f" #'find-file
  "C-f" #'find-file-read-only
  "s" #'save-buffer
  "r" #'counsel-recentf)

;; Window and Buffer management
(leader-b
  :states 'normal
  "b" #'switch-to-buffer
  "i" #'ibuffer
  "q" #'kill-buffer
  "k" #'delete-window)

(defkey :prefix "C-x w"
  "o" "C-x 1"
  "q" "C-x 0"
  "k" #'windmove-up
  "j" #'windmove-down
  "h" #'windmove-left
  "l" #'windmove-right
  "s" #'split-window-below
  "v" #'split-window-right)

(leader-w
  :states 'normal
  "o" "C-x 1"
  "q" "C-x 0"
  "k" #'windmove-up
  "j" #'windmove-down
  "h" #'windmove-left
  "l" #'windmove-right
  "s" #'split-window-below
  "v" #'split-window-right)

(alt-leader-b
  :states 'normal
  "b" #'switch-to-buffer
  "i" #'ibuffer
  "q" #'kill-buffer
  "k" #'delete-buffer)

(alt-leader-w
  :states 'normal
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

;; Help
(leader-h
  :states 'normal
  "t" 'counsel-load-theme
  "s" 'company-show-doc-buffer)

(alt-leader-h
  :states 'normal
  "t" 'counsel-load-theme
  "s" 'company-show-doc-buffer)

;; Treemacs
(leader-quote
  :states 'normal
  "" 'treemacs)

(alt-leader-quote
  :states 'normal
  "" 'treemacs)

;; Magit
(leader-g
  :states 'normal
  "g" 'magit-status
  "s" 'magit-stage-file
  "c" 'magit-commit
  "p" 'magit-push
  "C-p" 'magit-pull
  "b" 'magit-branch
  "i" 'magit-init
  "u" 'magit-unstage)

(alt-leader-g
  :states 'normal
  "g" 'magit-status
  "s" 'magit-stage-file
  "c" 'magit-commit
  "p" 'magit-push
  "C-p" 'magit-pull
  "b" 'magit-branch
  "i" 'magit-init
  "u" 'magit-unstage)

;; Projectile
(leader-p
  :states 'normal
  "." 'projectile-commander
  "p" 'projectile-switch-project
  "a" 'projectile-add-known-project
  "d" 'projectile-find-dir
  "C-d" 'projectile-dired
  "f" 'projectile-find-file)

(alt-leader-p
  :states 'normal
  "." 'projectile-commander
  "p" 'projectile-switch-project
  "a" 'projectile-add-known-project
  "d" 'projectile-find-dir
  "C-d" 'projectile-dired
  "f" 'projectile-find-file)

;; Emacs eval stuff
(alt-leader-e
  :states '(normal visual)
  :keymaps 'emacs-lisp-mode-map
  "e" 'eval-last-sexp
  "b" 'eval-buffer
  "r" 'eval-region
  "d" 'eval-defun
  ":" 'eval-expression)

(leader-e
  :states '(normal visual)
  :keymaps 'emacs-lisp-mode-map
  "e" 'eval-last-sexp
  "b" 'eval-buffer
  "r" 'eval-region
  "d" 'eval-defun
  ":" 'eval-expression)

;; Company
(leader-tab
  :states 'normal
  "" 'company-complete)

;; Smartparens
(evil-map*
 (:nv
  :keymaps smartparens-mode-map
  "w" sp-forward-sexp
  "W" sp-next-sexp
  "b" sp-backward-sexp
  "B" sp-previous-sexp)
 (:nv
  :keymaps smartparens-mode-map
  "w" sp-forward-sexp
  "W" sp-next-sexp
  "b" sp-backward-sexp
  "B" sp-previous-sexp)
 (:o
  :keymaps smartparens-mode-map
  "w" sp-forward-sexp
  "W" sp-next-sexp
  "b" sp-backward-sexp
  "B" sp-previous-sexp))

;; Keyboard macros
(leader-q
  :states 'normal
  "" (general-simulate-key "C-x C-k" :state 'emacs))

(alt-leader-q
  :states 'normal
  "" (general-simulate-key "C-x C-k" :state 'emacs))

;; Help system
(leader-h
  :states '(normal visual)
  "" (general-simulate-key "C-h" :state 'emacs))
