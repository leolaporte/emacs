;;; early-init.el --- Early initialization file for Emacs 27+  -*- lexical-binding: t; -*-

;;; Commentary:
;; This file is loaded before package.el and the GUI is initialized.
;; It's the proper place to disable package.el in favor of straight.el.

;;; Code:

;; Disable package.el in favor of straight.el
;; This must be set before package.el is loaded
(setq package-enable-at-startup nil)

;; Increase garbage collection threshold during startup
;; (Will be reset in init.el)
(setq gc-cons-threshold most-positive-fixnum)

;;; early-init.el ends here
