;; Fred Ross's Emacs customizations.

; My custom keybindings
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

; Set up my interface
(line-number-mode 1)
(column-number-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 1)

; Add packages and configure them
(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

(require 'color-theme)
(require 'color-theme-tango)

;; Org-mode configuration
(add-hook 'org-mode-hook 'my-org-customizations)
(defun my-org-customizations ()
  (auto-revert-mode)
  (setq truncate-lines nil))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.gpg\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.plan\\'" . org-mode))

;; My journal
(defun insert-time ()
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(setq journal-path "~/Dropbox/data/org")
(setq journal-base-name "technical_diary.org")

(defun append-journal-entry ()
  (interactive)
  (find-file (concat journal-path "/" journal-base-name))
  (end-of-buffer)
  (insert "\n\n")
  (insert "* ")
  (insert-time)
  (insert " "))

(global-set-key "\C-x\C-j" 'append-journal-entry)




(add-to-list 'default-frame-alist '(font . "Menlo-16"))

