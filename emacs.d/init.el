;;; summary --- Fred's emacs config
;;; Commentary:

;;; Code:
(setq user-full-name "Fred Ross")
(setq user-mail-address "fred@madhadron.com")

(defvar macintosh-p (string-equal system-type "darwin"))
(if macintosh-p
    (progn
      (setenv "PATH" (concat (getenv "PATH")
			     ":/usr/local/bin"))
      (add-to-list 'exec-path "/usr/local/bin")))

(require 'cl-lib)
(require 'package)
(setq package-archives 
      '(("ELPA" . "http://tromey.com/elpa/")
	("gnu" . "http://elpa.gnu.org/packages/")
	("melpa" . "http://melpa.org/packages/")
	("melpa-stable" . "http://stable.melpa.org/packages/")
	("marmalade" . "http://marmalade-repo.org/packages/")
	))

(package-initialize)


(let* ((packages '(cl
		   elpy
		   py-autopep8
           web-mode
           flycheck
           company
           company-quickhelp))
       (installed-p (cl-loop for pkg in packages
              when (not (package-installed-p pkg)) do (cl-return nil)
              finally (cl-return t))))
  (unless installed-p
    (message "%s" "Refreshing package database...")
    (package-refresh-contents)
    (dolist (pkg packages)
      (when (not (package-installed-p pkg))
    (package-install pkg)))))

(global-flycheck-mode)
 
(require 'company)
(require 'company-quickhelp)
(global-company-mode)
(setq company-idle-delay 0.05)
(setq company-quickhelp-delay 0.05)
(add-to-list 'company-dabbrev-code-modes 'web-mode)

;; Python flycheck
(elpy-enable)
(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))
(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)
;(elpy-use-ipython)
 
;; JavaScript/HTML/CSS
(setq flycheck-jscsrc (expand-file-name "~/murmur/hooks/jscsrc"))
(require 'web-mode)
(add-hook 'web-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)
	    (setq web-mode-markup-indent-offset 2)))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

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

(define-key global-map (kbd "<S-down-mouse-1>") 'mouse-save-then-kill)
(global-set-key (kbd "M-z") 'undo)
(global-set-key (kbd "M-x") 'kill-region)
(global-set-key (kbd "M-c") 'copy-region-as-kill)
(global-set-key (kbd "M-v") 'yank)
(global-set-key (kbd "M-RET") 'execute-extended-command)
(global-set-key (kbd "<s-return>") 'execute-extended-command)
(global-set-key (kbd "M-DEL") 'backward-kill-word)
(global-set-key (kbd "<s-backspace>") 'backward-kill-word)
(global-set-key (kbd "M-s") 'save-buffer)
(global-set-key (kbd "M-0") 'find-file)
(global-set-key (kbd "s-0") 'find-file)
(global-set-key (kbd "M-l") 'goto-line)

(global-unset-key (kbd "C-w"))
(global-unset-key (kbd "C-x C-f"))
(global-unset-key (kbd "C-x C-s"))

; Set up my interface
(line-number-mode 1)
(column-number-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 1)

;; ; Add packages and configure them
(let ((default-directory "~/.emacs.d/"))
  (normal-top-level-add-subdirs-to-load-path))

;; ;; Org-mode configuration
(add-hook 'org-mode-hook 'my-org-customizations)
(defun my-org-customizations ()
  (setq truncate-lines nil))

(require 'epa-file)
(epa-file-enable)

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.gpg\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.plan\\'" . org-mode))

;; My journal
(defun insert-time ()
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(defvar journal-path "~/data/org")
(defvar journal-base-name "technical_diary.org")

(defun switch-to-journal ()
  (interactive)
  (find-file (concat journal-path "/" journal-base-name))
  (goto-char (point-max)))
(global-set-key (kbd "<shift-f7>") 'switch-to-journal)

(defun append-journal-entry ()
  (interactive)
  (find-file (concat journal-path "/" journal-base-name))
  (goto-char (point-max))
  (insert "\n\n")
  (insert "* ")
  (insert-time)
  (insert " "))
(global-set-key (kbd "<f7>") 'append-journal-entry)

(defun append-bjournal-entry ()
  (interactive)
  (find-file (concat journal-path "/business_journal.org"))
  (goto-char (point-max))
  (insert "\n\n")
  (insert "* ")
  (insert-time)
  (insert " "))
(global-set-key (kbd "<f8>") 'append-bjournal-entry)


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
(global-set-key (kbd "s-/") 'eshell-here)

(provide 'init)
;;; init.el ends here
