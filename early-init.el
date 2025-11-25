;; -*- lexical-binding: t; -*-

;;; early-init.el --- Early initialization file for Emacs 27+
;; This file is loaded before package.el and the GUI is initialized.

;; Increase garbage collection threshold for faster startup
(setq gc-cons-threshold most-positive-fixnum)

;; Prefer newer versions of files (for compile-angel.el)
(setq load-prefer-newer t)

;; Enable native compilation (JIT) for better performance
(when (and (fboundp 'native-comp-available-p)
           (native-comp-available-p))
  (setq native-comp-jit-compilation t)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors nil)) ; reduce noise

;; Disable package.el in favor of elpaca
(setq package-enable-at-startup nil)

;;; early-init.el ends here
