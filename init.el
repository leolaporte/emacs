
;; Emacs configuration
;; Leo Laporte, 8 Dec 2020
;; Updated on Framework Fri 19 Nov 2021 03:45:58 PM PST
;; Cleaned up Tue 23 Nov 2021 03:40:42 PM PST

;; Based on https://github.com/flyingmachine/emacs-for-clojure
;; with additions from https://emacsredux.com/blog/2020/12/08/favorite-emacs-packages/
;; and Mickey Petersen's excellent "Mastering Emacs" https://www.masteringemacs.org/
;; and various suggestions from various contribs at https://reddit.com/r/emacs

;; Packages using straight.el package manager
;; https://github.com/raxod502/straight.el
;;;;

;; increase garbage collection threshold to 50MB to reduce time spent in GC
(setq gc-cons-threshold 50000000)

;; warn before opening giant files over 100MB
(setq large-file-warning-threshold 100000000)

;; finesse auto save and backup 
(setq auto-save-list-file-prefix ; Prefix for generating auto-save-list-file-name
      (expand-file-name ".auto-save-list/.saves-" user-emacs-directory)
      auto-save-default t        ; Auto-save every buffer that visits a file
      auto-save-timeout 20       ; Number of seconds between auto-save
      auto-save-interval 200)    ; Number of keystrokes between auto-saves

(setq backup-directory-alist       ; File name patterns and backup directory names.
      `(("." . ,(expand-file-name "backups" user-emacs-directory)))
      make-backup-files t          ; Backup of a file the first time it is saved.
      vc-make-backup-files nil     ; No backup of files under version contr
      backup-by-copying t          ; Don't clobber symlinks
      version-control t            ; Version numbers for backup files
      delete-old-versions t        ; Delete excess backup files silently
      kept-old-versions 6          ; Number of old versions to keep
      kept-new-versions 9          ; Number of new versions to keep
      delete-by-moving-to-trash t) ; Delete files to trash

(setq bookmark-default-file (expand-file-name "bookmark" user-emacs-directory))


;; use straight.el for package installation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-pull-package "melpa")  ; update repository
(straight-use-package 'use-package) ; preserve compatibility with all those use-package statements!

;; follow symlinks if necessary (can slow Emacs)
(setq find-file-visit-truename t)

;;;;
;; Customization
;; All the installed packages are in these files, purely for organizational purposes
;;;;

;; folder for customization files
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Install helm for completions, etc. -- config cribbed whole from its author
(load "init-helm.el")

;; Org-mode customizations
(load "org-mode.el")

;; Hard-to-categorize customizations like Magit
(load "misc.el")

;; Set up vterm for shell
(load "shell-integration.el")

;; For editing lisps
(load "lisp.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; Set up ssh-agent and gpg-agent
(load "keychain.el")

;; Set up blogging environment
(load "blog.el")

;; All set up, now start server for faster loading in future
(server-start)

