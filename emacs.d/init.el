; Global key customizations
(global-set-key "\C-x\C-m" 'execute-extended-command)
(global-set-key "\C-c\C-m" 'execute-extended-command)
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)

(global-set-key "\C-x\C-g" 'magit-status)
(line-number-mode 1)
(column-number-mode 1)
(scroll-bar-mode 0)
(tool-bar-mode 0)
(toggle-menu-bar-mode-from-frame t)


(add-to-list 'load-path "~/.emacs.d")
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)



(require 'color-theme)
(require 'color-theme-tango)

; Test IDO things -  from http://www.jaydonnell.com/blog/2011/10/07/setting-up-aquamacs-for-clojure-and-general-goodness/
(require 'ido)
(ido-mode t)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)

;; Display ido results vertically, rather than horizontally
(setq ido-decorations (quote ("\n-> " "" "\n " "\n ..." "[" "]" " [No match]" " [Matched]" " [Not readable]" " [Too big]" " [Confirm]")))
(defun ido-disable-line-trucation () (set (make-local-variable 'truncate-lines) nil))
(add-hook 'ido-minibuffer-setup-hook 'ido-disable-line-trucation)

(require 'anything)
(require 'anything-match-plugin)
(require 'anything-config)

(setq recentf-max-saved-items 500)

(defun anything-c-sources-git-project-for (pwd)
  (loop for elt in
        '(("Modified files (%s)" . "--modified")
          ("Untracked files (%s)" . "--others --exclude-standard")
          ("All controlled files in this project (%s)" . ""))
        collect
        `((name . ,(format (car elt) pwd))
          (init . (lambda ()
                    (unless (and ,(string= (cdr elt) "") ;update candidate buffer every time except for that of all project files
                                 (anything-candidate-buffer))
                      (with-current-buffer
                          (anything-candidate-buffer 'global)
                        (insert
                         (shell-command-to-string
                          ,(format "git ls-files $(git rev-parse --show-cdup) %s"
                                   (cdr elt))))))))
          (candidates-in-buffer)
          (type . file))))

(defun anything-git-project ()
  (interactive)
  (let* ((pwd (shell-command-to-string "echo -n `pwd`"))
         (sources (anything-c-sources-git-project-for pwd)))
    (anything-other-buffer sources
     (format "*Anything git project in %s*" pwd))))


; Now my insane org-mode modifications


(add-to-list 'load-path "~/.emacs.d/site-lisp")
(require 'typopunct)
(typopunct-change-language 'english t)


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
  (turn-off-auto-fill)
  (longlines-mode 0))

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
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
                    "DejaVu Sans Mono-18")


(defun insert-time ()
  (interactive)
  (insert (format-time-string "%Y-%m-%d-%R")))

(defun journal () 
  (interactive) 
  (find-file "~/journal.org")
  (end-of-buffer)
  (insert "\n\n")
  (insert "* ")
  (insert-time)
  (insert "\n\n"))
(global-set-key "\C-x\C-j" 'journal)


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

(setq org-log-done 'time)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(setq org-todo-keywords '((sequence "DOING"  "BLOCKED" "PENDING" "|" "FINISHED")
                          (sequence "WAITING" "TODO" | "DONE")))

(setq org-agenda-custom-commands
      '(("w" "Waiting fors..." ((todo "WAITING")))
        ("y" "@Anywhere todos" ((tags-todo "@anywhere")))
        ("c" "@Computer todos" ((tags-todo "@computer")))
        ("e" "@Errands todos" ((tags-todo "@errands")))
        ("f" "@Fremont todos" ((tags-todo "@fremont")))
        ("s" "@Sonja's todos" ((tags-todo "@sonjas")))
        ("k" "@Work todos" ((tags-todo "@work")))
        ("p" "Current projects" ((todo "DOING")))))


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
 '(org-agenda-custom-commands (quote (("w" "Waiting for items" todo "WAITING" nil nil))))
 '(org-agenda-files (quote ("~/data/org/projects.org"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
