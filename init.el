;; emacs configuration
;; leo laporte, 8 Dec 2020
;; Updated on Framework Fri 19 Nov 2021 03:45:58 PM PST
;; Cleaned up Tue 23 Nov 2021 03:40:42 PM PST
;; reorganized and updated with the help of Claude Code 2 November 2025

;; see .emacs.d/keybindings.md for custom keybindings

;; Based on https://github.com/flyingmachine/emacs-for-clojure
;; with additions from https://emacsredux.com/blog/2020/12/08/favorite-emacs-packages/
;; and Mickey Petersen's excellent "Mastering Emacs" https://www.masteringemacs.org/
;; and various suggestions from various contribs at https://reddit.com/r/emacs

;; warn before opening giant files over 100MB
(setq large-file-warning-threshold 100000000)

;; Define when to check for package modifications,
;; for improved straight.el startup time.
(setq straight-check-for-modifications '(check-on-save find-when-checking))

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

;; To update package repositories, run manually: M-x straight-pull-all
;; or M-x straight-pull-package

;;;;
;; Customization
;; All the installed packages are in these files, purely for organizational purposes
;;;;

;; Store emacs's customizations elsewhere
(setq custom-file "~/.emacs.d/customizations/custom.el")
(load custom-file)

;; folder for customization files
(add-to-list 'load-path "~/.emacs.d/customizations")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; These customizations make it easier to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; Completion framework
(load "completion.el")

;; Hard-to-categorize customizations like Magit
(load "misc.el")

;; For editing lisps
(load "lisp.el")

;; Set up vterm for shell
(load "shell-integration.el")

;; Advent of Code helpers
(load "aoc-helpers.el")

;; Everybody Codes helpers
(load "ec-helpers.el")

