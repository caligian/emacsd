(leader-b
  "b" #'switch-to-buffer
  "i" #'ibuffer
  "q" #'kill-buffer
  "r" #'counsel-recentf)

(leader-w
  "k" #'windmove-up
  "j" #'windmove-down
  "h" #'windmove-left
  "l" #'windmove-right
  "s" #'split-window-below
  "v" #'split-window-right)

(defkey
  "C-=" #'align
  "C-+" #'align-regexp)

(leader-h "t" 'counsel-load-theme)

(global-unset-key (kbd "C-S-t"))
(global-set-key (kbd "C-S-t") 'treemacs)
(global-unset-key (kbd "C-S-g"))
(global-set-key (kbd "C-S-g") 'magit-status)

(leader-p
  "." 'projectile-commander
  "p" 'projectile-switch-project
  "a" 'projectile-add-known-project
  "d" 'projectile-find-dir
  "C-d" 'projectile-dired
  "f" 'projectile-find-file)

(leader-l "s" 'lsp-ivy-workspace-symbol)

