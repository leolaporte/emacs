;; Lisp specific packages
;; including sly and lispy

;; project navigation
;; projectile everywhere!
(straight-use-package 'projectile)
(projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Racket mode support
;; (straight-use-package 'racket-mode)
;; (setq racket-program "opt/homebrew/bin/racket")

;; Common Lisp support - btw I use sly
(straight-use-package 'sly)
;; (setq global-helm-sly-mode t)
;; (straight-use-package 'helm-sly)

;; launch whenever a lisp files is opened
(add-hook 'sly-mode-hook
	  (lambda ()
	    (unless (sly-connected-p)
	      (save-excursion (sly)))))

;; Auto-complete
;; https://github.com/auto-complete
(straight-use-package 'auto-complete)
(straight-use-package 'ac-sly)  ; add support for Sly
(add-hook 'sly-mode-hook 'set-up-sly-ac)
(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'sly-mrepl-mode))
(global-auto-complete-mode t)

;; load the fantastic lispy for fast CL navigation
;; https://github.com/abo-abo/lispy
(straight-use-package 'lispy)
(add-hook 'sly-mode-hook (lambda () (lispy-mode 1)))

;; always split windows vertically
;; (I like the REPL on the right on most displays)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; locate lisp package
(if (eq system-type 'darwin)
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")  ;; MacOS
  (setq inferior-lisp-program "/usr/bin/sbcl"));; Linux

;; colorful parenthesis matching
(straight-use-package 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(require 'rainbow-delimiters)
(set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
(set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
(set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
(set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
(set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
(set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
(set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
(set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
(set-face-foreground 'rainbow-delimiters-depth-9-face "#666")  ; dark gray

;; Highlights matching parenthesis
(show-paren-mode 1)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
