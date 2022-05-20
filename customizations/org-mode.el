;;;; Org-mode Customizations
;;;; Leo Laporte, April 2022

(with-eval-after-load 'org       
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook #'visual-line-mode)) ; word wrap by default

;; where my org files at?
;; I store these in my synced Sync/Journal folder, symlinked to ~/org/
(setq org-agenda-files (directory-files-recursively "~/org/" "\\.org$"))
(setq org-directory "~/org")

;; ask for note when closing task
(setq org-log-done 'note)

;; lisp source code support
(org-babel-do-load-languages
 'org-babel-load-languages
 '((lisp . t)))

(setf org-babel-lisp-eval-fn 'sly-eval)

;; configure capture
(global-set-key (kbd "C-c c") #'org-capture)
(setq org-default-notes-file (concat org-directory "/notes.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/org/todo.org" "Tasks")
         "* TODO %?\n  %i\n  %a")
        ("j" "Journal" entry (file+datetree "~/org/journal.org")
         "* %?\nEntered on %U\n  %i\n  %a")
	("s" "Shopping List" entry (file+datetree "~/org/shopping.org")
         "* %?\nEntered on %U\n  %i\n  %a")
	("i" "Inbox" entry (file+datetree "~/org/inbox.org")
	 "* %?\nEntered on %U\n  %i\n  %a")))

(global-set-key (kbd "C-c l") #'org-store-link)
(global-set-key (kbd "C-c a") #'org-agenda)
(global-set-key (kbd "C-c b") #'org-iswitchb)

;; Refile related settings from http://doc.norang.ca/org-mode.html
;; Targets include this file and any file contributing to the agenda - up to 9 levels deep
(setq org-refile-targets (quote ((nil :maxlevel . 9)
                                 (org-agenda-files :maxlevel . 9))))

;;;; Refile settings
;; Exclude DONE state tasks from refile targets
(defun bh/verify-refile-target ()
  "Exclude todo keywords with a done state from refile targets"
  (not (member (nth 2 (org-heading-components)) org-done-keywords)))

(setq org-refile-target-verify-function 'bh/verify-refile-target)
