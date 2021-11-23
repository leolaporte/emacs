;; Emacs configuration
;; Leo Laporte, 8 Dec 2020
;; Updated on Framework Fri 19 Nov 2021 03:45:58 PM PST
;; Cleaned up Tue 23 Nov 2021 03:40:42 PM PST

;; Based on https://github.com/flyingmachine/emacs-for-clojure
;; with additions from https://emacsredux.com/blog/2020/12/08/favorite-emacs-packages/
;; and Mickey Petersen's excellent "Mastering Emacs" https://www.masteringemacs.org/
;; and various suggestions from the excellent contribs at https://reddit.com/r/emacs

;; Packages using straight.el package manager
;; https://github.com/raxod502/straight.el
;;;;

;; increase garbage collection threshold to 50MB to reduce time spent in GC
(setq gc-cons-threshold 50000000)

;; warn before opening giant files over 100MB
(setq large-file-warning-threshold 100000000)

;; use straight.el for package installation
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  ((unless (file-exists-p bootstrap-file)
     with-current-buffer
     (url-retrieve-synchronously
      "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
      'silent 'inhibit-cookies)
     (goto-char (point-max))
     (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; preserve compatibility with all those use-package statements!

;;;;
;; Customization
;; All the installed packages are in these files, purely for organizational purposes
;;;;

;; folder for customization files
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Install helm for completions, etc. -- config cribbed whole from its author
(load "init-helm.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations like Magit
(load "misc.el")

;; Set up vterm for shell
(load "shell-integration.el")

;; For editing lisps
(load "lisp.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("31f1723fb10ec4b4d2d79b65bcad0a19e03270fe290a3fc4b95886f18e79ac2f" default)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
