(require 'general)
(defalias 'defkey 'general-define-key)
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

(use-package dash)

(use-package s)

(use-package ht)

(use-package feebleline
  :demand
  :config
  (feebleline-mode 1))

(use-package base16-theme
  :demand
  :config
  (load-theme 'base16-atelier-sulphurpool t))

(use-package general
  :demand
  :config
  (require 'general)
  (defalias 'defkey 'general-define-key))

(use-package ivy
  :demand
  :bind (("M-'" . ivy-resume))
  :config
  (ivy-mode 1))

(use-package counsel
  :demand
  :config
  (counsel-mode 1))
  
(use-package magit
  :defer t
  :commands magit-status)

(use-package smartparens
  :demand
  :bind (("C-M-y" . sp-copy-sexp)
	 ("C-M-u" . sp-up-sexp)
	 ("C-M-d" . sp-down-sexp)
	 ("C-M-]" . sp-forward-barf-sexp)
	 ("C-M-[" . sp-backward-barf-sexp)
	 ("C-M-}" . sp-slurp-hybrid-sexp)
	 ("C-M-{" . sp-backward-slurp-sexp)
	 ("C-)" . sp-wrap-round)
	 ("C-}" . sp-wrap-curly)
	 ("M-f" . sp-forward-sexp)
	 ("M-b" . sp-backward-sexp)
	 ("M-d" . sp-kill-word)
	 ("C-M-a" . sp-beginning-of-sexp)
	 ("C-M-e" . sp-end-of-sexp)
	 ("M-a" . sp-beginning-of-next-sexp)
	 ("M-e" . sp-end-of-next-sexp)
	 ("C-*" . sp-unwrap-sexp)
	 ("C-k" . sp-kill-hybrid-sexp)
	 ("C-M-d" . sp-kill-sexp)
	 ("C-M-j" . sp-join-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package wrap-region
  :config
  (wrap-region-mode 1))

(use-package treemacs
  :defer t
  :commands treemacs)

(use-package lsp-mode
  :defer t
  :config
  (add-hooks '(python-mode-hook lsp-mode)
	     '(ruby-mode-hook lsp-mode)))

(use-package yasnippet
  :defer t
  :commands yas-global-mode
  :config
  (yas-global-mode))

(use-package flycheck
  :defer t)

(use-package pipenv
  :defer t)

(use-package json-mode
  :defer t)

(use-package lua-mode
  :defer t) 

(use-package ess
  :defer t)

(use-package julia-mode
  :defer t)

(use-package raku-mode
  :defer t)

(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.3)
  
  :config
  (which-key-mode 1))

(use-package vterm
  :defer t)

(use-package rg
  :defer t
  :commands rg)

(use-package projectile
  :defer t
  :commands (projectile-mode
	     projectile-commander
	     projectile-switch-project
	     projectile-add-known-project
	     projectile-find-dir
	     projectile-dired
	     projectile-find-file)
  :config
  (setq projectile-completion-system 'ivy
	projectile-enable-caching t
	projectile-sort-order 'recently-active
	projectile-indexing-method 'alien)
  (projectile-mode t))

(use-package company
  :defer t
  :bind (("M-SPC" . company-complete))
  :commands global-company-mode
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode 1))

(use-package haskell-mode)

(use-package lsp-pyright
  :hook (python-mode . (lambda nil
			 (require 'lsp-pyright)
			 (lsp))))

(use-package lsp-ivy
  :defer t)
