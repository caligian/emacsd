;; Aliases
(defalias 'defn 'cl-defun)

;; Load default theme here
(load-theme 'wombat)

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
(setq inhibit-startup-screen t)

;; Set font
(set-face-attribute 'default nil
		    :family "Tamsyn"
		    :height 130)

;; Load packages with their configs
(load "~/.emacs.d/utils.el")
(load "~/.emacs.d/packages.el")

