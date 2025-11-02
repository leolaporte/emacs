;; Miscellaneous extensions
;; Leo Laporte Tue 23 Nov 2021 03:32:38 PM PST

;; git integration
;; https://magit.vc
(straight-use-package 'magit)
(require 'magit)

;; notmuch email
;; (require 'notmuch)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; No need for ~ files when editing
(setq create-lockfiles nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)
