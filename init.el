;; Emacs configuration
;; Leo Laporte, 8 Dec 2020
;; based on https://github.com/flyingmachine/emacs-for-clojure
;; with additions from https://emacsredux.com/blog/2020/12/08/favorite-emacs-packages/
;; 
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
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package) ; preserve compatibility with all those use-package statements!

;; Install packages

;; Install Helm completions (instead of IDO or IVY)
;; https://emacs-helm.github.io
(straight-use-package 'helm)
(helm-mode 1)
(setq completion-styles '(flex))

;; my favorite terminal emulator package - requires emacs built with module support
(straight-use-package 'vterm)

;; Common Lisp support - I like sly instead of slime
(straight-use-package 'sly)
(setq global-helm-sly-mode t)
(straight-use-package 'helm-sly)

;; locate lisp package
(if (eq system-type 'darwin)
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")  ;; MacOS
  (setq inferior-lisp-program "/usr/bin/sbcl"))      ;; Linux

;; minor mode for Emacs that displays the key bindings following your currently entered incomplete command 
(straight-use-package 'which-key)
(which-key-mode)

;; project navigation
(straight-use-package 'projectile)

;; colorful parenthesis matching
(straight-use-package 'rainbow-delimiters)

;; edit html tags like sexps
;; (straight-use-package 'tagedit)

;; git integration
;; https://magit.vc
(straight-use-package 'magit)

;; replaces emacs undo with a tree-based system
;;https://elpa.gnu.org/packages/undo-tree.html
(straight-use-package 'undo-tree)
(global-undo-tree-mode)

;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")

;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Helm config - from its author
(load "init-helm.el")

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
