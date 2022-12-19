(straight-use-package 'use-package)

(use-package dash)

(use-package s)

(use-package f)

(use-package ht)

(use-package bind-key)

(use-package doom-modeline
  :demand t
  :config
  (doom-modeline-mode 1))

(use-package kaolin-themes
  :demand t
  :config
  (load-theme 'kaolin-valley-dark t))

(use-package evil
  :demand t

  :init
  (setq evil-move-beyond-eol t)
  (setq evil-undo-system 'undo-fu)
  (setq evil-split-window-below t)
  (setq evil-split-window-right t)

  :config
  (dolist (buf '(("^\\*[^*]*" . emacs)
		 ("\\*scratch" . normal)))
    (push buf evil-buffer-regexps))
  (evil-mode 1)
  (evil-set-initial-state 'vterm-mode 'emacs))

(use-package general
  :demand t

  :config
  (defalias 'defkey 'general-define-key))

(use-package undo-fu)

(use-package ivy
  :demand
  :defines personal-keybindings
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
  :defines smartparens-mode-map
  :commands smartparens-mode-map
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
  (add-hook 'ruby-mode-hook (lambda nil (require 'smartparens-ruby)))
  (add-hook 'python-mode-hook (lambda nil (require 'smartparens-python)))
  (add-hook 'lua-mode-hook (lambda nil (require 'smartparens-lua)))
  (smartparens-global-mode 1))

(use-package wrap-region
  :config
  (wrap-region-mode 1))

(use-package treemacs
  :commands treemacs)

(use-package yasnippet
  :defer t
  :hook ((python-mode lua-mode emacs-lisp-mode lisp-mode ruby-mode perl-mode sh-mode) . yas-minor-mode))

(use-package flycheck
  :defer t)

(use-package pipenv
  :defer t
  :hook (python-mode . pipenv-mode))

(use-package json-mode)

(use-package lua-mode)

(use-package which-key
  :demand
  :init
  (setq which-key-idle-delay 0.3)
  
  :config
  (which-key-mode 1))

(use-package vterm
  :commands vterm
  :config
  (add-hook 'vterm-mode-hook (lambda nil
			       (local-unset-key (kbd "M-SPC")))))

(use-package rg
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
  :defines personal-keybindings
  :bind (("<f1>" . company-complete))
  :config
  (setq company-idle-delay 0.3)
  (global-company-mode 1))

(use-package haskell-mode)

(use-package avy
  :defines personal-keybindings
  :bind (("C-'" . avy-goto-word-1)
	 ("C-." . avy-goto-line)
	 ("C->" . avy-goto-line-below)
	 ("C-<" . avy-goto-line-above)))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l")

  :hook ((python-mode ruby-mode) . lsp))

(use-package lsp-ui
  :after lsp-mode)

(use-package flycheck)

(use-package lsp-treemacs
  :after (lsp-mode treemacs))

(use-package lsp-ivy)

(use-package vimish-fold
  :after evil
  :config
  (vimish-fold-global-mode 1))

(use-package evil-owl
  :after evil
  :config
  (setq evil-owl-max-string-length 500)
  (add-to-list 'display-buffer-alist
               '("*evil-owl*"
                 (display-buffer-in-side-window)
                 (side . bottom)
                 (window-height . 0.3)))
  (evil-owl-mode))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode 1))

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package tree-sitter-langs)

(use-package tree-sitter
  :hook ((python-mode lua-mode ruby-mode) . tree-sitter-mode))

(use-package evil-textobj-tree-sitter
  :after evil
  :config
  (defkey :keymaps 'evil-inner-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.inner")
    "b" (evil-textobj-tree-sitter-get-textobj "block.inner")
    "." (evil-textobj-tree-sitter-get-textobj "call.inner")
    "c" (evil-textobj-tree-sitter-get-textobj "class.inner")
    ";" (evil-textobj-tree-sitter-get-textobj "comment.inner")
    "?" (evil-textobj-tree-sitter-get-textobj "conditional.inner")
    "i" (evil-textobj-tree-sitter-get-textobj "loop.inner")
    "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))

  (defkey :keymaps 'evil-outer-text-objects-map
    "f" (evil-textobj-tree-sitter-get-textobj "function.outer")
    "b" (evil-textobj-tree-sitter-get-textobj "block.outer")
    "." (evil-textobj-tree-sitter-get-textobj "call.outer")
    "c" (evil-textobj-tree-sitter-get-textobj "class.outer")
    ";" (evil-textobj-tree-sitter-get-textobj "comment.outer")
    "?" (evil-textobj-tree-sitter-get-textobj "conditional.outer")
    "i" (evil-textobj-tree-sitter-get-textobj "loop.outer")
    "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer")))
