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
;; https://www.masteringemacs.org/ and suggestions from various
;; contribs at https://reddit.com/r/emacs and lately with considerable
;; help in refactoring and optimization from Claude code!  Really
;; helpful. See Claude.md for more info.

;; warn before opening giant files over 100MB
(setq large-file-warning-threshold 100000000)

;; Bootstrap elpaca package manager
(defvar elpaca-installer-version 0.11)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                   ,@(when-let* ((depth (plist-get order :depth)))
                                                       (list (format "--depth=%d" depth) "--no-single-branch"))
                                                   ,(plist-get order :repo) ,repo))))
                  ((zerop (call-process "git" nil buffer t "checkout"
                                        (or (plist-get order :ref) "--"))))
                  (emacs (concat invocation-directory invocation-name))
                  ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                        "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                  ((require 'elpaca))
                  ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode))

;; Block until current queue is processed
(elpaca-wait)

;; Ensure packages are installed by default
(setq use-package-always-ensure t)

;; To update packages, run: M-x elpaca-update-all or M-x elpaca-update

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
