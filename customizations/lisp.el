;; Lisp specific packages
;; including sly, paraedit

;; project navigation
;; projectile everywhere!
(straight-use-package 'projectile)
(projectile-global-mode)

;; Common Lisp support - btw I use sly
(straight-use-package 'sly)
(setq global-helm-sly-mode t)
(straight-use-package 'helm-sly)

;; locate lisp package
(if (eq system-type 'darwin)
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")  ;; MacOS
  (setq inferior-lisp-program "/usr/bin/sbcl"));; Linux

;; makes handling lisp expressions much, much easier
;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(straight-use-package 'paredit)
;; Mac keybindings using fn key
(add-hook 'paredit-mode-hook
          (lambda ()
            (define-key paredit-mode-map (kbd "<prior>") 'paredit-forward-slurp-sexp)
            (define-key paredit-mode-map (kbd "<next>") 'paredit-backward-slurp-sexp)
            (define-key paredit-mode-map (kbd "<home>") 'paredit-backward-barf-sexp)
            (define-key paredit-mode-map (kbd "<end>") 'paredit-forward-barf-sexp)))

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; colorful parenthesis matching
(straight-use-package 'rainbow-delimiters)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
