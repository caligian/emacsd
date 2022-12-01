(use-package dash)

(use-package s)

(use-package ht)

(use-package feebleline
  :demand t
  :config
  (feebleline-mode 1))

(use-package base16-theme
  :demand t
  :config
  (load-theme 'base16-gotham t))

(use-package general
  :demand t
  :config
  (require 'general)
  (defalias 'defkey 'general-define-key))

(use-package ivy
  :demand
  :bind (("M-'" . ivy-resume))
  :config
  (ivy-mode 1))

(use-package counsel
  :demand t
  :config
  (counsel-mode 1))
  
(use-package magit
  :defer t
  :commands magit-status)

(use-package smartparens
  :demand t
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
  :hook ((python-mode ruby-mode) . lsp-mode))

(use-package yasnippet
  :defer t
  :commands yas-global-mode
  :config
  (yas-global-mode))

(use-package flycheck
  :defer t)

(use-package pipenv
  :defer t
  :commands pipenv-mode)

(use-package json-mode
  :defer t)

(use-package lua-mode
  :defer t
  :hook (lua-mode . lua-mode))

(use-package ess
  :defer t
  :hook (r-mode . ess-r-mode))

(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.3)
  
  :config
  (which-key-mode 1))

(use-package vterm)

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

(use-package geiser
  :hook (scheme-mode . geiser))

(use-package geiser-guile
  :hook (scheme-mode . geiser-guile))
