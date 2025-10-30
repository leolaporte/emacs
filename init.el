;; emacs configuration
;; leo laporte, 8 Dec 2020
;; Updated on Framework Fri 19 Nov 2021 03:45:58 PM PST
;; Cleaned up Tue 23 Nov 2021 03:40:42 PM PST

;; Based on https://github.com/flyingmachine/emacs-for-clojure
;; with additions from https://emacsredux.com/blog/2020/12/08/favorite-emacs-packages/
;; and Mickey Petersen's excellent "Mastering Emacs" https://www.masteringemacs.org/
;; and various suggestions from various contribs at https://reddit.com/r/emacs

;; Install packages using straight.el package manager
;; https://github.com/raxod502/straight.el
;;;;

;; increase garbage collection threshold to 50MB to reduce time spent in GC
(setq gc-cons-threshold 50000000)

;; warn before opening giant files over 100MB
(setq large-file-warning-threshold 100000000)

;; Define when to check for package modifications,
;; for improved straight.el startup time.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

;; Disable package.el in favor of straight.el
(setq package-enable-at-startup nil)

;; use straight.el for package installation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; preserve compatibility with all those use-package statements!

;; Configure use-package to use straight.el by default
(use-package straight
  :custom
  (straight-use-package-by-default t)) ; implicit :straight t in use-package calls

(straight-pull-package "melpa")  ; update repository
(straight-pull-package "elpa")

;; follow symlinks if necessary (can slow Emacs)
;; (setq find-file-visit-truename t)

;;;;
;; Customization
;; All the installed packages are in these files, purely for organizational purposes
;;;;

;; Store emacs's customizations elsewhere
(setq custom-file "~/.emacs.d/customizations/custom.el")
(load custom-file)

;; folder for customization files
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Org-mode customizations
;; (load "org-mode.el")

;; Install helm for completions, etc. -- config cribbed whole from its author
;; (load "init-helm.el")

;; New completion framework
(load "completion.el")

;; Hard-to-categorize customizations like Magit
(load "misc.el")

;; Set up vterm for shell
(load "shell-integration.el")

;; For editing lisps
(load "lisp.el")

;; for Clojure
;; (load "setup-clojure.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; All set up, now start server for faster loading in future
(require 'server)
(unless (server-running-p)
  (server-start))
