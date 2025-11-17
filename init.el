;; -*- lexical-binding: t; -*-

;; Main emacs configuration (calls other files in ./customizations/
;; leo laporte, 8 Dec 2020

;; Updated on Framework Fri 19 Nov 2021 03:45:58 PM PST
;; Cleaned up Tue 23 Nov 2021 03:40:42 PM PST
;; reorganized and updated with the help of Claude Code 2 November 2025

;; see .emacs.d/keybindings.md for custom keybindings

;; Based on https://github.com/flyingmachine/emacs-for-clojure with
;; additions from
;; https://emacsredux.com/blog/2020/12/08/favorite-emacs-packages/ and
;; Mickey Petersen's excellent "Mastering Emacs"
;; https://www.masteringemacs.org/ and various suggestions from
;; various contribs at https://reddit.com/r/emacs and lately with
;; considerable help in refactoring and optimization from Claude code!
;; Really helpful. See Claude.md for more info.

;; warn before opening giant files over 100MB
(setq large-file-warning-threshold 100000000)

;; Initialize package.el
(require 'package)

;; Add MELPA repository
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; Initialize package system
(package-initialize)

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Enable use-package
(require 'use-package)

;; Ensure packages are installed by default
(setq use-package-always-ensure t)

;; To update packages, run: M-x package-list-packages, then U x

;;;;
;; Customization All the installed packages are in these files, purely
;; for organizational purposes
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

;; For editing lisps
(load "lisp.el")

;; Hard-to-categorize customizations like Magit
(load "misc.el")

;; Set up vterm for shell
(load "shell-integration.el")

;; Advent of Code helpers
(load "aoc-helpers.el")

;; Everybody Codes helpers
(load "ec-helpers.el")
