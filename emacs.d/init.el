;; Fred Ross's Emacs customizations.
(setq user-full-name "Fred Ross")
(setq user-mail-address "fred@madhadron.com")

(require 'cl)
(require 'package) 
(package-initialize)
(add-to-list 'package-archives 
	     '("melpa-stable" . "http://stable.melpa.org/packages/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))


(let* ((packages '(magit
		   flycheck
		   go-mode
		   color-theme
		   color-theme-tango))
       (installed-p (loop for pkg in packages
			  when (not (package-installed-p pkg)) do (return nil)
			  finally (return t))))
  (unless installed-p
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (dolist (pkg packages)
      (when (not (package-installed-p pkg))
	(package-install pkg)))))

(setq inhibit-splash-screen t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

(scroll-bar-mode 1)
(tool-bar-mode -1)
(menu-bar-mode 1)

(delete-selection-mode t)
(transient-mark-mode t)
(setq x-select-enable-clipboard t)
(setq indent-tabs-mode nil)
(global-auto-revert-mode 1)

(defalias 'yes-or-no-p 'y-or-n-p)

(setq macintosh-p (string-equal system-type "darwin"))

(if macintosh-p
    (custom-set-variables '(ns-command-modifier (quota meta))))

(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-x") 'kill-region)
(if macintosh-p
    (global-set-key (kbd "M-c") 'ns-copy-including-secondary)
    (global-set-key (kbd "M-c") 'copy-region-as-kill))
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-RET") 'execute-extended-command)
(global-set-key (kbd "M-DEL") 'backward-kill-word)
(global-set-key (kbd "M-s") 'save-buffer)


(global-unset-key (kbd "C-w"))

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
(add-hook 'org-mode-hook 'my-org-customizations)
(defun my-org-customizations ()
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
(global-set-key (kbd "<f7>") 'append-journal-entry)

;; Go
(setenv "GOPATH" (expand-file-name "~/murmur/signalsd"))
(add-to-list 'load-path "~/.emacs.d/" t)
(require 'go-mode-autoloads)
(add-hook 'before-save-hook 'gofmt-before-save)
(add-hook 'go-mode-hook '(lambda () 
  (local-set-key (kbd "M-]") 'godef-jump)
  (local-set-key (kbd "M-[") 'pop-global-mark)
  (linum-mode t)))

;;goflymake
(let ((flymake-path (concat (file-name-as-directory (getenv "GOPATH"))
                            "src/github.com/dougm/goflymake")))
  (if (file-directory-p flymake-path)
      (progn
        (add-to-list 'load-path flymake-path)
        (require 'go-flymake)
        (require 'go-flycheck))
    (message "go-flymake not found.")))test.go



;; Terminal
(require 'eshell)
(require 'em-smart)
(setq eshell-where-to-jump 'begin)
(setq eshell-review-quick-commands nil)
(setq eshell-smart-space-goes-to-end t)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
    (file-name-directory (buffer-file-name))
  default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

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

(global-set-key (kbd "M-/") 'eshell-here)

