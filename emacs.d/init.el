;; Fred Ross's Emacs customizations.
(require 'package) 
(add-to-list 'package-archives '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(package-initialize)
(require 'ergoemacs-mode)
(setq ergoemacs-theme nil) ;; Uses Standard Ergoemacs keyboard theme
(setq ergoemacs-keyboard-layout "us") ;; Assumes QWERTY keyboard layout
(ergoemacs-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ergoemacs-ctl-c-or-ctl-x-delay 0.2)
 '(ergoemacs-handle-ctl-c-or-ctl-x (quote both))
 '(ergoemacs-ini-mode t)
 '(ergoemacs-keyboard-layout "us")
 '(ergoemacs-mode nil)
 '(ergoemacs-smart-paste t)
 '(ergoemacs-theme "standard")
 '(ergoemacs-theme-options nil)
 '(ergoemacs-use-menus t)
 '(initial-scratch-message ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.

")
 '(org-CUA-compatible nil)
 '(org-special-ctrl-a/e nil)
 '(org-support-shift-select nil)
 '(scroll-error-top-bottom nil)
 '(set-mark-command-repeat-pop nil)
 '(shift-select-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(global-auto-revert-mode 1)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key (kbd "<mode-line> <C-mouse-3>") 'mouse-split-window-vertically)
(global-set-key (kbd "<C-S-mouse-1>") 'kill-this-buffer)

; Set up my interface
(line-number-mode 1)
(column-number-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 1)

;; ; Add packages and configure them
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

(require 'color-theme)
(require 'color-theme-tango)

;; ;; Org-mode configuration
;; (add-hook 'org-mode-hook 'my-org-customizations)
;; (defun my-org-customizations ()
;;   (setq truncate-lines nil))

;; (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.gpg\\'" . org-mode))
;; (add-to-list 'auto-mode-alist '("\\.plan\\'" . org-mode))

;; ;; My journal
;; (defun insert-time ()
;;   (interactive)
;;   (insert (format-time-string "<%Y-%m-%d %a>")))

;; (setq journal-path "~/Dropbox/data/org")
;; (setq journal-base-name "technical_diary.org")

;; (defun append-journal-entry ()
;;   (interactive)
;;   (find-file (concat journal-path "/" journal-base-name))
;;   (end-of-buffer)
;;   (insert "\n\n")
;;   (insert "* ")
;;   (insert-time)
;;   (insert " "))


;; Go
(add-to-list 'load-path "~/.emacs.d/" t)
(require 'go-mode-autoloads)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook '(lambda () 
  (local-set-key (kbd "M-]") 'godef-jump)
  (local-set-key (kbd "M-[") 'pop-global-mark)))

;;goflymake
(add-to-list 'load-path "~/murmur/signalsd/src/github.com/dougm/goflymake")
(require 'go-flymake)
(require 'go-flycheck)


;; Terminal
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

 (eval-after-load "em-ls"
    '(progn
       (defun ted-eshell-ls-find-file-at-point (point)
         "RET on Eshell's `ls' output to open files."
         (interactive "d")
         (find-file (buffer-substring-no-properties
                     (previous-single-property-change point 'help-echo)
                     (next-single-property-change point 'help-echo))))

       (defun pat-eshell-ls-find-file-at-mouse-click (event)
         "Middle click on Eshell's `ls' output to open files.
 From Patrick Anderson via the wiki."
         (interactive "e")
         (ted-eshell-ls-find-file-at-point (posn-point (event-end event))))

       (let ((map (make-sparse-keymap)))
         (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point)
         (define-key map (kbd "<mouse-2>") 'pat-eshell-ls-find-file-at-mouse-click)
         (defvar ted-eshell-ls-keymap map))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, mouse-2: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)))


;; (add-to-list 'load-path "~/.emacs.d/yaml-mode")
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.sls$" . yaml-mode))


