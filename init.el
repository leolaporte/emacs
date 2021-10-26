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

;; my favorite terminal emulator package
(straight-use-package 'vterm)

;; Common Lisp support 
(straight-use-package 'sly)
(setq inferior-lisp-program "/opt/homebrew/bin/sbcl")
;;(setq slime-contribs '(slime-fancy))
;;(load (expand-file-name "~/quicklisp/slime-helper.el"))

;; turn on agressive-indent-mode for all major modes
;; https://github.com/Malabarba/aggressive-indent-mode/blob/master/README.md
(straight-use-package 'aggressive-indent)
(global-aggressive-indent-mode 1)
(add-to-list 'aggressive-indent-excluded-modes 'html-mode)
(add-to-list 'aggressive-indent-excluded-modes 'python-mode)

;; minor mode for Emacs that displays the key bindings following your currently entered incomplete command 
(straight-use-package 'which-key)
(which-key-mode)

;; makes handling lisp expressions much, much easier
;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(straight-use-package 'paredit)

;; Increase the selected region by semantic units
;; https://github.com/magnars/expand-region.el
(straight-use-package 'expand-region)
;; bind to C-=
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
;; clojure-mode

;; extra syntax highlighting for clojure
;; clojure-mode-extra-font-locking

;; integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
;; cider

;; allow ido usage in as many contexts as possible. see
;; customizations/navigation.el line 23 for a description
;; of ido
;; (straight-use-package 'ido-completing-read+)
;; and add flx (also see navigation.el)
;; (straight-use-package 'flx-ido)

;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(straight-use-package 'smex)

;; project navigation
(straight-use-package 'projectile)

;; colorful parenthesis matching
(straight-use-package 'rainbow-delimiters)

;; edit html tags like sexps
;; (straight-use-package 'tagedit)

;; git integration
;; https://magit.vc
(straight-use-package 'magit)

;; minibuffer completion/filtering/sorting framework
;; https://github.com/abo-abo/swiper
;; (straight-use-package 'ivy)
;; (global-set-key (kbd "C-:") 'avy-goto-char)
;; (global-set-key (kbd "C-'") 'avy-goto-char-2)
;; (global-set-key (kbd "M-g e") 'avy-goto-word-0)

;; simple but effective sorting and filtering for Emacs
;; https://github.com/raxod502/prescient.el
;; (straight-use-package 'ivy-prescient)

;; a Collection of Ridiculously Useful eXtensions for Emacs
;; https://github.com/bbatsov/crux
;; see navigation.el for special crux keys
(straight-use-package 'crux)

;; replaces emacs undo with a tree-based system
;;https://elpa.gnu.org/packages/undo-tree.html
(straight-use-package 'undo-tree)
(global-undo-tree-mode)

;; markdown mode
(straight-use-package 'markdown-mode)

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
(if (eq system-type 'darwin)
    (straight-use-package 'exec-path-from-shell))

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

;; Langauage-specific
;; (load "setup-clojure.el")
;; (load "setup-js.el")

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
