;; -*- lexical-binding: t; -*-

;; Miscellaneous extensions
;; Leo Laporte Tue 23 Nov 2021 03:32:38 PM PST

;; Garbage Collector Magic Hack - improves GC performance
;; https://github.com/emacsmirror/gcmh
(use-package gcmh
  :config
  (gcmh-mode 1))

;; Transient - Required by Magit (ensure latest version)
;; https://github.com/magit/transient
(use-package transient
  :demand t)

;; Magit - A Git porcelain inside Emacs
;; https://magit.vc/
(use-package magit
  :after transient
  :bind ("C-x g" . magit-status)
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))

;; Automatically byte-compile and native-compile Emacs Lisp files
;; https://github.com/jamescherti/compile-angel.el
(use-package compile-angel
  :demand t
  :custom
  (compile-angel-verbose nil)  ; set to t for debugging
  :config
  ;; Exclude init files from automatic compilation to avoid recursion issues
  (add-to-list 'compile-angel-excluded-files "/init.el")
  (add-to-list 'compile-angel-excluded-files "/early-init.el")
  ;; Enable automatic compilation on load
  (compile-angel-on-load-mode 1))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)
