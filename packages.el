(use-package dash)

(use-package s)

(use-package ht)

(use-package simple-modeline
  :config
  (simple-modeline-mode))

(use-package general
  :config
  (defalias 'defkey 'general-define-key)

  (defkey 
    :prefix "C-c C-b"
    "C-b" #'switch-to-buffer
    "C-i" #'ibuffer
    "C-k" "C-x 0"
    "C-q" #'kill-buffer)

  (defkey
    :prefix "C-c C-w"
    "C-k" #'windmove-up
    "C-j" #'windmove-down
    "C-h" #'windmove-left
    "C-k" #'windmove-right
    "C-s" #'split-window-below
    "C-v" #'split-window-right
    "C-o" "C-x 1"
    "C-q" "C-x 0"))

(use-package ivy
  :commands ivy-mode
  :config
  (ivy-mode 1))

(use-package counsel
  :commands counsel-mode
  :bind (("C-c C-b r" . counsel-recentf))
  :config
  (counsel-mode 1))

(use-package magit
  :defer t
  :commands magit-status
  :bind (("C-c g" . magit-status)))

(use-package smartparens
  :bind (("C-M-]" . sp-forward-barf-sexp)
	 ("C-M-[" . sp-backward-barf-sexp)
	 ("C-M-}" . sp-slurp-hybrid-sexp)
	 ("C-M-{" . sp-backward-slurp-sexp)
	 ("C-)" . sp-wrap-round)
	 ("C-}" . sp-wrap-curly)
	 ("M-f" . sp-forward-sexp)
	 ("M-b" . sp-backward-sexp)
	 ("M-d" . sp-kill-sexp)
	 ("C-M-a" . sp-beginning-of-sexp)
	 ("C-M-e" . sp-end-of-sexp)
	 ("M-a" . sp-beginning-of-next-sexp)
	 ("M-e" . sp-end-of-next-sexp))
  :config
  (require 'smartparens-config)
  (smartparens-global-mode 1))

(use-package wrap-region
  :config
  (wrap-region-mode 1))

(use-package treemacs
  :defer t
  :commands treemacs
  :bind (("C-c '" . treemacs)))

(use-package lsp-mode
  :defer t
  :config
  (add-hooks '(python-mode-hook lsp-mode)
	     '(ruby-mode-hook lsp-mode)))

(use-package rainbow-delimiters
  :defer t
  :commands rainbow-delimeters-mode
  :config
  (rainbow-delimiters-mode t))

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
  :commands which-key-mode
  :config
  (which-key-mode 1))

(use-package vterm
  :defer t)

(use-package rg
  :bind
  (("C-c /" . rg)))

(use-package projectile
  :defer t
  :commands projectile-mode
  
  :bind
  (("C-c C-p ." . projectile-commander)
   ("C-c C-p p" . projectile-switch-project)
   ("C-c C-p C-a" . projectile-add-known-project)
   ("C-c C-p d" . projectile-find-dir)
   ("C-c C-p C-d" . projectile-dired))

  :config
  (projectile-mode t)
  (setq projectile-completion-system 'ivy
	projectile-enable-caching t
	projectile-sort-order 'recently-active
	projectile-indexing-method 'alien))

(use-package company
  :bind (("M-SPC" . company-complete))
  :commands global-company-mode
  :config
  (setq company-idle-delay 0)
  (global-company-mode t))

(use-package company-ctags
  :config
  (with-eval-after-load 'company
    (company-ctags-auto-setup)))

(use-package ctags-update
  :commands ctags-global-auto-update-mode
  :config
  (ctags-global-auto-update-mode 1)
  (setq ctags-update-prompt-create-tags nil))

(use-package counsel-etags
  :bind (("M-]" . counsel-etags-find-tag-at-point))
  :init
  (add-hook 'prog-mode-hook
        (lambda ()
          (add-hook 'after-save-hook
            'counsel-etags-virtual-update-tags 'append 'local)))
  :config
  (setq counsel-etags-update-interval 60)
  (push "build" counsel-etags-ignore-directories))
