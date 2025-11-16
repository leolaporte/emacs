;; -*- lexical-binding: t; -*-

;;; early-init.el --- Early initialization file for Emacs 27+
;; This file is loaded before package.el and the GUI is initialized.
;; It's the proper place to disable package.el in favor of straight.el.

;; Disable package.el in favor of straight.el
;; This must be set before package.el is loaded
(setq package-enable-at-startup nil)

;; Increase garbage collection threshold
(setq gc-cons-threshold most-positive-fixnum)

;;; early-init.el ends here
