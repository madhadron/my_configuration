; Global key customizations
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-xm" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)
(global-set-key "\M-p\M-p" 'magit-status)
(line-number-mode 1)
(column-number-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(menu-bar-mode 1)

(let ((default-directory "~/.emacs.d/"))
      (normal-top-level-add-subdirs-to-load-path))

(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(require 'color-theme)
(require 'color-theme-tango)

;; ; IDO things -  from http://www.jaydonnell.com/blog/2011/10/07/setting-up-aquamacs-for-clojure-and-general-goodness/
;; (require 'ido)
;; (ido-mode t)
;; (setq ido-enable-prefix nil
;;       ido-enable-flex-matching t
;;       ido-auto-merge-work-directories-length nil
;;       ido-create-new-buffer 'always
;;       ido-use-filename-at-point 'guess
;;       ido-use-virtual-buffers t
;;       ido-handle-duplicate-virtual-buffers 2
;;       ido-max-prospects 10)

;; ;; Display ido results vertically, rather than horizontally
;; (setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
;; (defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
;; (add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

;; (require 'anything)
;; (require 'anything-match-plugin)
;; (require 'anything-config)

;; (setq recentf-max-saved-items 500)

;; (defun anything-c-sources-git-project-for (pwd)
;;   (loop for elt in
;;         '(("Modified files (%s)" . "--modified")
;;           ("Untracked files (%s)" . "--others --exclude-standard")
;;           ("All controlled files in this project (%s)" . ""))
;;         collect
;;         `((name . ,(format (car elt) pwd))
;;           (init . (lambda ()
;;                     (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
;;                                  (anything-candidate-buffer))
;;                       (with-current-buffer
;;                           (anything-candidate-buffer 'global)
;;                         (insert
;;                          (shell-command-to-string
;;                           ,(format "git ls-files $(git rev-parse --show-cdup) %s"
;;                                    (cdr elt))))))))
;;           (candidates-in-buffer)
;;           (type . file))))

;; (defun anything-git-project ()
;;   (interactive)
;;   (let* ((pwd (shell-command-to-string "echo -n `pwd`"))
;;          (sources (anything-c-sources-git-project-for pwd)))
;;     (anything-other-buffer sources
;;      (format "*Anything git project in %s*" pwd))))


;;;;;;;;;;;;
;; Typopunct
;;;;;;;;;;;;
(require 'typopunct)
(typopunct-change-language 'english t)

(global-set-key "\M-p\M-m" 'typopunct-mode)

(defconst typopunct-minus (decode-char 'ucs #x2212))
(defconst typopunct-pm    (decode-char 'ucs #xB1))
(defconst typopunct-mp    (decode-char 'ucs #x2213))
(defadvice typopunct-insert-typographical-dashes
  (around minus-or-pm activate)
  (cond
   ((or (eq (char-before) typopunct-em-dash)
        (looking-back "\\([[:blank:]]\\|^\\)\\^"))
    (delete-char -1)
    (insert typopunct-minus))
   ((looking-back "[^[:blank:]]\\^")
    (insert typopunct-minus))
   ((looking-back "+/")
    (progn (replace-match "")
           (insert typopunct-pm)))
   ((looking-back "|-*")
    (progn (replace-match "")
           (insert "|---")))
   (t ad-do-it)))
(defun typopunct-insert-mp (arg)
  (interactive "p")
  (if (and (= 1 arg) (looking-back "-/"))
      (progn (replace-match "")
             (insert typopunct-mp))
    (self-insert-command arg)))
(define-key typopunct-map "+" 'typopunct-insert-mp)

(defconst typopunct-ellipsis (decode-char 'ucs #x2026))
(defconst typopunct-middot   (decode-char 'ucs #xB7)) ; or 2219
(defun typopunct-insert-ellipsis-or-middot (arg)
  "Change three consecutive dots to a typographical ellipsis mark."
  (interactive "p")
  (cond
   ((and (= 1 arg)
         (eq (char-before) ?^))
    (delete-char -1)
    (insert typopunct-middot))
   ((and (= 1 arg)
         (eq this-command last-command)
         (looking-back "\\.\\."))
    (replace-match "")
    (insert typopunct-ellipsis))
   (t
    (self-insert-command arg))))
(define-key typopunct-map "." 'typopunct-insert-ellipsis-or-middot)

(defconst typopunct-times (decode-char 'ucs #xD7))
(defun typopunct-insert-times (arg)
  (interactive "p")
  (if (and (= 1 arg) (looking-back "\\([[:blank:]]\\|^\\)\\^"))
      (progn (delete-char -1)
             (insert typopunct-times))
    (self-insert-command arg)))
(define-key typopunct-map "x" 'typopunct-insert-times)

(defconst typopunct-prime  (decode-char 'ucs #x2032))
(defconst typopunct-dprime (decode-char 'ucs #x2033))
(defconst typopunct-tprime (decode-char 'ucs #x2034))
(defadvice typopunct-insert-quotation-mark (around primes activate)
  (cond
   ((or mark-active
        (not (eq last-command-char ?')))
    ad-do-it)
   ((eq (char-before) ?^)
    (delete-char -1)
    (insert typopunct-prime))
   ((eq (char-before) typopunct-prime)
    (delete-char -1)
    (insert typopunct-dprime))
   ((eq (char-before) typopunct-dprime)
    (delete-char -1)
    (insert typopunct-tprime))
   (t ad-do-it)))

(defadvice typopunct-insert-quotation-mark (around wrap-region activate)
  (let* ((lang (or (get-text-property (point) 'typopunct-language)
                   typopunct-buffer-language))
         (omark (if single
                    (typopunct-opening-single-quotation-mark lang)
                  (typopunct-opening-quotation-mark lang)))
         (qmark (if single
                    (typopunct-closing-single-quotation-mark lang)
                  (typopunct-closing-quotation-mark lang))))
    (cond
     (mark-active
      (let ((skeleton-end-newline nil)
            (singleo (typopunct-opening-single-quotation-mark lang))
            (singleq (typopunct-closing-single-quotation-mark lang)))
        (if (> (point) (mark))
            (exchange-point-and-mark))
        (save-excursion
          (while (re-search-forward (regexp-quote (string omark)) (mark) t)
            (replace-match (regexp-quote (string singleo)) nil nil)))
        (save-excursion
          (while (re-search-forward (regexp-quote (string qmark)) (mark) t)
            (replace-match (regexp-quote (string singleq)) nil nil)))
        (skeleton-insert (list nil omark '_ qmark) -1)))
     ((looking-at (regexp-opt (list (string omark) (string qmark))))
      (forward-char 1))
     (t ad-do-it))))

(require 'iso-transl)
(iso-transl-define-keys
 `(("^0" . ,(vector (decode-char 'ucs #x2070)))
   ("^4" . ,(vector (decode-char 'ucs #x2074))) ; 1-3 already defined
   ("^5" . ,(vector (decode-char 'ucs #x2075)))
   ("^6" . ,(vector (decode-char 'ucs #x2076)))
   ("^7" . ,(vector (decode-char 'ucs #x2077)))
   ("^8" . ,(vector (decode-char 'ucs #x2078)))
   ("^9" . ,(vector (decode-char 'ucs #x2079)))
   ("^+" . ,(vector (decode-char 'ucs #x207A)))
   ("^-" . ,(vector (decode-char 'ucs #x207B)))
   ("^=" . ,(vector (decode-char 'ucs #x207C)))
   ("^(" . ,(vector (decode-char 'ucs #x207D)))
   ("^)" . ,(vector (decode-char 'ucs #x207E)))
   ("_0" . ,(vector (decode-char 'ucs #x2080)))
   ("_1" . ,(vector (decode-char 'ucs #x2081)))
   ("_2" . ,(vector (decode-char 'ucs #x2082)))
   ("_3" . ,(vector (decode-char 'ucs #x2083)))
   ("_4" . ,(vector (decode-char 'ucs #x2084)))
   ("_5" . ,(vector (decode-char 'ucs #x2085)))
   ("_6" . ,(vector (decode-char 'ucs #x2086)))
   ("_7" . ,(vector (decode-char 'ucs #x2087)))
   ("_8" . ,(vector (decode-char 'ucs #x2088)))
   ("_9" . ,(vector (decode-char 'ucs #x2089)))
   ("_+" . ,(vector (decode-char 'ucs #x208A)))
   ("_-" . ,(vector (decode-char 'ucs #x208B)))
   ("_=" . ,(vector (decode-char 'ucs #x208C)))
   ("_(" . ,(vector (decode-char 'ucs #x208D)))
   ("_)" . ,(vector (decode-char 'ucs #x208E)))))

(defun count-words-region (start end)
    "Print number of words in the region."
    (interactive "r")
    (save-excursion
      (save-restriction
        (narrow-to-region start end)
        (goto-char (point-min))
        (count-matches "\\sw+"))))
(defalias 'word-count-region 'count-words-region)
(require 'wc-mode)

(remove-hook 'org-mode-hook 'turn-on-auto-fill)
(remove-hook 'text-mode-hook 'turn-on-auto-fill)
(add-hook 'org-mode-hook 'my-org-customizations)
(defun my-org-customizations ()
  (require 'typopunct)
  (typopunct-change-language 'english)
  (typopunct-mode 1)
  (wc-mode)
  (flyspell-mode)
  (auto-revert-mode)
  (turn-off-auto-fill)
  (longlines-mode 0))

(setq org-src-fontify-natively t)
(setq org-directory "~/Dropbox/data/org")
(setq org-mobile-inbox-for-pull "~/Dropbox/data/org/flagged.org")
(setq org-mobile-directory "~/Dropbox/MobileOrg")
(setq org-default-notes-file (concat org-directory "/todos.org"))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.txt\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.gpg\\'" . org-mode))
(add-to-list 'auto-mode-alist '("\\.plan\\'" . org-mode))
(add-to-list 'auto-mode-alist '("bashrc\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("bash_profile\\'" . sh-mode))
(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

(set-default 'truncate-lines nil)
(setq truncate-partial-width-windows nil)
(global-visual-line-mode)

(setq ispell-program-name "aspell")

(load "~/.emacs.d/elpa/haskell-mode-2.8.0/haskell-site-file.el")
(add-hook 'haskell-mode-hook 'turn-on-haskell-doc-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indent)
(setq haskell-font-lock-symbols t)


(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (emacs-lisp . t)
   (css . t)
   (haskell . t)
   (latex . t)
   (lisp . t)
   (python . t)
   (scheme . t)
   (sh . t)
   (sql . t)
   (sqlite .t )))

(set-face-attribute 'default nil :font
                    "DejaVu Sans Mono-28")

(setq org-export-html-style-include-default t)


(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R")))


; SQL mode
(require 'sql)
(defun my-sql-save-history-hook ()
    (let ((lval 'sql-input-ring-file-name)
          (rval 'sql-product))
      (if (symbol-value rval)
          (let ((filename 
                 (concat "~/.emacs.d/sql/"
                         (symbol-name (symbol-value rval))
                         "-history.sql")))
            (set (make-local-variable lval) filename))
        (error
         (format "SQL history will not be saved because %s is nil"
                 (symbol-name rval))))))
(add-hook 'sql-interactive-mode-hook 'my-sql-save-history-hook)

  (defun sql-add-newline-first (output)
    "Add newline to beginning of OUTPUT for `comint-preoutput-filter-functions'"
    (concat "\n" output))

  (defun sqli-add-hooks ()
    "Add hooks to `sql-interactive-mode-hook'."
    (add-hook 'comint-preoutput-filter-functions
              'sql-add-newline-first))

(add-hook 'sql-interactive-mode-hook 'sqli-add-hooks)
(defcustom sql-sqlite-program "sqlite3"
  "Command to start SQLite."
  :type 'file
  :group 'SQL)

(defun sql-make-smart-buffer-name ()
  "Return a string that can be used to rename a SQLi buffer.

This is used to set `sql-alternate-buffer-name' within
`sql-interactive-mode'."
  (or (and (boundp 'sql-name) sql-name)
      (concat (if (not(string= "" sql-server))
                  (concat
                   (or (and (string-match "[0-9.]+" sql-server) sql-server)
                       (car (split-string sql-server "\\.")))
                   "/"))
              sql-database)))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (setq sql-alternate-buffer-name (sql-make-smart-buffer-name))
            (sql-rename-buffer)))

(setq sql-connection-alist
      '((iontorrent
         (sql-product 'postgres)
         (sql-server "localhost")
         (sql-user "fredross")
         (sql-database "iontorrent")
         (sql-port 5432))))


(defun sql-connect-preset (name)
  "Connect to a predefined SQL connection listed in `sql-connection-alist'"
  (eval `(let ,(cdr (assoc name sql-connection-alist))
    (flet ((sql-get-login (&rest what)))
      (sql-product-interactive sql-product)))))

(defun sql-iontorrent ()
  (interactive)
  (sql-connect-preset 'iontorrent))

(setq sql-association-alist
      '(("IonTorrent" ("localhost" "fredross" "rf1ru!3&nky" "iontorrent"))))

;; LaTeX
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-masterr nil)

(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)

(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))

(defun fc-eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case
      nil (progn
            (insert (current-kill 0))
            (insert " = ")
            (prin1 (eval (read (current-kill 0))) (current-buffer)))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key (kbd "C-c e") 'fc-eval-and-replace)
(setq org-log-done 'time)
(setq org-tag-alist '(("@anywhere" . ?a)
                      ("@errands" . ?e)
                      ("@home" . ?h)
                      ("@computer" . ?c)
                      ("@work" . ?w)
                      ("@email" . ?m)))

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords '((sequence "DOING"  "BLOCKED" "PENDING" "|" "FINISHED")
                          (sequence "WAITING" "TODO" "|" "DONE")))

(setq org-agenda-dim-blocked-tasks t)
(setq org-enforce-todo-checkbox-dependencies t)
(setq org-agenda-custom-commands
      (cons
       (list "g" "Global todo list"
             (mapcar (lambda (x) (list 'tags-todo (car x) '((org-agenda-prefix-format "")))) org-tag-alist))
       '(("h" "Todos at home"
          ((agenda "")
           (tags-todo "@anywhere|@computer|@home" ((org-agenda-prefix-format "")))))
         ("w" "Todos at work"
          ((agenda "")
           (tags-todo "@anywhere|@computer|@work" ((org-agenda-prefix-format "")))))
         ("W" "Waiting for"
          ((todo "WAITING" ((org-agenda-prefix-format "")))))
         ("e" "Errands"
          ((tags-todo "@errands")))
         ("c" "Communications"
          ((tags-todo "@email" ((org-agenda-prefix-format "")))
           (tags-todo "@phone" ((org-agenda-prefix-format ""))))))))

(setq org-enforce-todo-dependencies t)
(setq org-todo-keyword-faces
      '(("WAITING-FOR" . org-warning)
        ("TODO" . org-warning)
        ("DOING" . org-warning)
        ("PENDING" . "yellow")))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(js-indent-level 4)
 '(org-agenda-files (quote ("~/Dropbox/data/org/todos.org" "~/Dropbox/data/org/projects.org"))))

; From http://nflath.com/2010/03/org-mode-2/
(org-clock-persistence-insinuate)
(setq org-clock-idle-time 15)
(setq org-clock-out-remove-zero-time-clocks t)
(setq org-clock-persist 'history)

(setq org-capture-templates
      '(("t" "Todo" entry (id "27DAE93A-D2D9-44F3-856D-D9CC81DC108D")
         "** TODO %?")
        ))

(defun my-org-mode-ask-effort ()
  "Ask for an effort estimate when clocking in."
  (unless (org-entry-get (point) "Effort")
    (let ((effort
           (completing-read
            "Effort: "
            (org-entry-get-multivalued-property (point) "Effort"))))
      (unless (equal effort "")
        (org-set-property "Effort" effort)))))
(add-hook 'org-clock-in-prepare-hook 'my-org-mode-ask-effort)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-default ((t (:inherit default :family "Lucida Grande"))))
 '(org-level-1 ((t (:inherit outline-1 :slant normal :weight normal :height 1.266 :family "Lucida Grande"))))
 '(org-level-2 ((t (:inherit outline-2 :slant normal :weight normal :height 1.125 :family "Lucida Grande"))))
 '(org-level-3 ((t (:inherit outline-3 :slant normal :weight bold :height 1.0 :family "Lucida Grande"))))
 '(org-table ((t (:foreground "LightSkyBlue" :family "Monaco")))))

;; My journal
(defun insert-time ()
  (interactive)
  (insert (format-time-string "<%Y-%m-%d %a>")))

(defun journal ()
  (interactive)
  (find-file "~/Dropbox/data/org/technical_diary.org")
  (end-of-buffer)
  (insert "\n\n")
  (insert "* ")
  (insert-time)
  (insert " "))

(global-set-key "\C-x\C-j" 'journal)

;; Encrypted files
(require 'epa-file)
(epa-file-enable)


(defvar visual-wrap-column nil)

(defun set-visual-wrap-column (new-wrap-column &optional buffer)
  "Force visual line wrap at NEW-WRAP-COLUMN in BUFFER (defaults
    to current buffer) by setting the right-hand margin on every
    window that displays BUFFER.  A value of NIL or 0 for
    NEW-WRAP-COLUMN disables this behavior."
  (interactive (list (read-number "New visual wrap column, 0 to disable: " (or visual-wrap-column fill-column 0))))
  (if (and (numberp new-wrap-column)
           (zerop new-wrap-column))
      (setq new-wrap-column nil))
  (with-current-buffer (or buffer (current-buffer))
    (visual-line-mode t)
    (set (make-local-variable 'visual-wrap-column) new-wrap-column)
    (add-hook 'window-configuration-change-hook 'update-visual-wrap-column nil t)
    (let ((windows (get-buffer-window-list)))
      (while windows
        (when (window-live-p (car windows))
          (with-selected-window (car windows)
            (update-visual-wrap-column)))
        (setq windows (cdr windows))))))

(defun update-visual-wrap-column ()
  (if (not visual-wrap-column)
      (set-window-margins nil nil)
    (let* ((current-margins (window-margins))
           (right-margin (or (cdr current-margins) 0))
           (current-width (window-width))
           (current-available (+ current-width right-margin)))
      (if (<= current-available visual-wrap-column)
          (set-window-margins nil (car current-margins))
        (set-window-margins nil (car current-margins)
                            (- current-available visual-wrap-column))))))
