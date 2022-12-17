;; Aliases
(defalias 'defn 'cl-defun)

;; Basic modes
(global-display-line-numbers-mode 1)
(electric-indent-mode 1)
(global-visual-line-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(eldoc-mode 1)

(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(setq c-basic-offset 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq-default cursor-type 'box)
(setq inhibit-startup-screen t)

;; Set font
(set-face-attribute 'default nil
		    :family "Droid Sans Mono"
		    :height 130)

;; Use C-, as the user prefix key
(global-unset-key (kbd "<menu>"))

;; Load important non-package functions
(load "~/.emacs.d/utils")
(append-exec-path-from-shell "~/.zshrc")

;; Load packages with their configs
(load "~/.emacs.d/packages")
(load "~/.emacs.d/keybindings")
(load "~/.emacs.d/repl")
