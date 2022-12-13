(require 'general)

(defalias 'defkey 'general-define-key)

;; Escape will be used as the alternative leader key
;; This is an alternative to not having <menu>
(global-unset-key (kbd "<escape>"))

;; Create definers for inconvenience
(general-create-definer alt-leader-tab :prefix "<escape> <tab>")
(general-create-definer alt-leader-a :prefix "<escape> a")
(general-create-definer alt-leader-b :prefix "<escape> b")
(general-create-definer alt-leader-c :prefix "<escape> c")
(general-create-definer alt-leader-d :prefix "<escape> d")
(general-create-definer alt-leader-e :prefix "<escape> e")
(general-create-definer alt-leader-f :prefix "<escape> f")
(general-create-definer alt-leader-g :prefix "<escape> g")
(general-create-definer alt-leader-h :prefix "<escape> h")
(general-create-definer alt-leader-i :prefix "<escape> i")
(general-create-definer alt-leader-j :prefix "<escape> j")
(general-create-definer alt-leader-k :prefix "<escape> k")
(general-create-definer alt-leader-l :prefix "<escape> l")
(general-create-definer alt-leader-m :prefix "<escape> m")
(general-create-definer alt-leader-n :prefix "<escape> n")
(general-create-definer alt-leader-o :prefix "<escape> o")
(general-create-definer alt-leader-p :prefix "<escape> p")
(general-create-definer alt-leader-q :prefix "<escape> q")
(general-create-definer alt-leader-r :prefix "<escape> r")
(general-create-definer alt-leader-s :prefix "<escape> s")
(general-create-definer alt-leader-t :prefix "<escape> t")
(general-create-definer alt-leader-u :prefix "<escape> u")
(general-create-definer alt-leader-v :prefix "<escape> v")
(general-create-definer alt-leader-w :prefix "<escape> w")
(general-create-definer alt-leader-x :prefix "<escape> x")
(general-create-definer alt-leader-y :prefix "<escape> y")
(general-create-definer alt-leader-z :prefix "<escape> z")
(general-create-definer alt-leader-/ :prefix "<escape> /")

(general-create-definer leader-tab :prefix "<menu> <tab>")
(general-create-definer leader-a :prefix "<menu> a")
(general-create-definer leader-b :prefix "<menu> b")
(general-create-definer leader-c :prefix "<menu> c")
(general-create-definer leader-d :prefix "<menu> d")
(general-create-definer leader-e :prefix "<menu> e")
(general-create-definer leader-f :prefix "<menu> f")
(general-create-definer leader-g :prefix "<menu> g")
(general-create-definer leader-h :prefix "<menu> h")
(general-create-definer leader-i :prefix "<menu> i")
(general-create-definer leader-j :prefix "<menu> j")
(general-create-definer leader-k :prefix "<menu> k")
(general-create-definer leader-l :prefix "<menu> l")
(general-create-definer leader-m :prefix "<menu> m")
(general-create-definer leader-n :prefix "<menu> n")
(general-create-definer leader-o :prefix "<menu> o")
(general-create-definer leader-p :prefix "<menu> p")
(general-create-definer leader-q :prefix "<menu> q")
(general-create-definer leader-r :prefix "<menu> r")
(general-create-definer leader-s :prefix "<menu> s")
(general-create-definer leader-t :prefix "<menu> t")
(general-create-definer leader-u :prefix "<menu> u")
(general-create-definer leader-v :prefix "<menu> v")
(general-create-definer leader-w :prefix "<menu> w")
(general-create-definer leader-x :prefix "<menu> x")
(general-create-definer leader-y :prefix "<menu> y")
(general-create-definer leader-z :prefix "<menu> z")
(general-create-definer leader-/ :prefix "<menu> /")

;; Define your keybindings here
;; Search
(leader-/
  "/" 'rg
  "o" 'occur
  "p" 'projectile-rg)

(alt-leader-/
  "/" 'rg
  "o" 'occur
  "p" 'projectile-rg)

;; File management
(leader-f
  "f" #'find-file
  "C-f" #'find-file-read-only
  "s" #'save-buffer
  "r" #'counsel-recentf)

(alt-leader-f
  "f" #'find-file
  "C-f" #'find-file-read-only
  "s" #'save-buffer
  "r" #'counsel-recentf)

;; Window and Buffer management
(leader-b
  "b" #'switch-to-buffer
  "i" #'ibuffer
  "q" #'kill-buffer)

(leader-w
  "o" "C-x 1"
  "q" "C-x 0"
  "k" #'windmove-up
  "j" #'windmove-down
  "h" #'windmove-left
  "l" #'windmove-right
  "s" #'split-window-below
  "v" #'split-window-right)

(alt-leader-b
  "b" #'switch-to-buffer
  "i" #'ibuffer
  "q" #'kill-buffer
  "r" #'counsel-recentf)

(alt-leader-w
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
(leader-h "t" 'counsel-load-theme)
(alt-leader-h "t" 'counsel-load-theme)

;; Treemacs
(global-unset-key (kbd "C-S-t"))
(global-set-key (kbd "C-S-t") 'treemacs)

;; Magit
(global-unset-key (kbd "C-S-g"))
(global-set-key (kbd "C-S-g") 'magit-status)

(leader-g
  "g" 'magit-status
  "s" 'magit-stage-file
  "c" 'magit-commit
  "p" 'magit-push
  "C-p" 'magit-pull
  "b" 'magit-branch
  "i" 'magit-init
  "u" 'magit-unstage)

(alt-leader-g
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
  "." 'projectile-commander
  "p" 'projectile-switch-project
  "a" 'projectile-add-known-project
  "d" 'projectile-find-dir
  "C-d" 'projectile-dired
  "f" 'projectile-find-file)

(alt-leader-p
  "." 'projectile-commander
  "p" 'projectile-switch-project
  "a" 'projectile-add-known-project
  "d" 'projectile-find-dir
  "C-d" 'projectile-dired
  "f" 'projectile-find-file)

;; LSP 
(leader-l "s" 'lsp-ivy-workspace-symbol)
(alt-leader-l "s" 'lsp-ivy-workspace-symbol)

;; Geiser
(alt-leader-r )

;; Slime
(alt-leader-r
  :keymaps 'slime-mode-map
  "e" 'slime-eval-last-expression
  "b" 'slime-eval-buffer
  "r" 'slime-eval-region
  "d" 'slime-eval-defun
  "m" 'slime-macroexpand-all
  "v" (lambda nil
	(interactive)
	(split-window-and-switch-to-buffer :v (car (get-buffer-by-regexp "slime-repl"))))
  "s" (lambda nil
	(interactive)
	(split-window-and-switch-to-buffer :s (car (get-buffer-by-regexp "slime-repl")))))

(leader-r
  :keymaps 'slime-mode-map
  "e" 'slime-eval-last-expression
  "b" 'slime-eval-buffer
  "r" 'slime-eval-region
  "d" 'slime-eval-defun
  "m" 'slime-macroexpand-all
  "v" (lambda nil
	(interactive)
	(split-window-and-switch-to-buffer :v (car (get-buffer-by-regexp "slime-repl"))))
  "s" (lambda nil
	(interactive)
	(split-window-and-switch-to-buffer :s (car (get-buffer-by-regexp "slime-repl")))))

(alt-leader-m
  :keymaps 'slime-mode-map
  "!" 'slime
  "h" 'slime-hyperspec-lookup
  "H" 'slime-describe-symbol
  "f" 'slime-describe-function
  "C-c" 'slime-interrupt
  "l" 'slime-load-file
  "c" 'slime-compile-file
  "TAB" 'slime-fuzzy-indent-and-complete-symbol)

(leader-m
  :keymaps 'slime-mode-map
  "!" 'slime
  "h" 'slime-hyperspec-lookup
  "H" 'slime-describe-symbol
  "f" 'slime-describe-function
  "C-c" 'slime-interrupt
  "l" 'slime-load-file
  "c" 'slime-compile-file
  "TAB" 'slime-fuzzy-indent-and-complete-symbol)
