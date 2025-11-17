;; -*- lexical-binding: t; -*-

;; Lisp specific packages
;; including sly paredit and lispy
;; Leo Laporte 5 Sept 2023-17 Nov 2025
;; see .emacs.d/keybindings.md for custom keybindings

;; First locate lisp package depending on OS
(if (eq system-type 'darwin)
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl") ;; MacOS
  (setq inferior-lisp-program "/usr/bin/sbcl")) ;; otherwise Linux

;; project navigation -- projectile everywhere!
(use-package projectile
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map)))

;; nice directory trees
(use-package treemacs)

(use-package treemacs-projectile
  :after (treemacs projectile))

;; Common Lisp support - btw I use sly
(use-package sly
  :config
  (add-to-list 'sly-contribs 'sly-quicklisp 'append)
  (add-to-list 'sly-contribs 'sly-asdf 'append)
  (add-to-list 'sly-contribs 'sly-macrostep 'append))

;; Sly extensions for enhanced Common Lisp development
(use-package sly-quicklisp
  :after sly)

(use-package sly-asdf
  :after sly)

(use-package sly-macrostep
  :after sly)

;; launch Sly whenever a lisp file is opened
(add-hook 'sly-mode-hook
	  (lambda ()
	    (unless (sly-connected-p)
	      (save-excursion (sly)))))

;; Enhanced Sly completion with corfu
(add-hook 'sly-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'sly-complete-symbol
                               #'cape-dabbrev
                               #'cape-file)))))

(add-hook 'sly-mrepl-mode-hook
          (lambda ()
            (setq-local completion-at-point-functions
                        (list (cape-capf-super
                               #'sly-complete-symbol
                               #'cape-dabbrev)))))

;; Sly REPL enhancements
(setq sly-complete-symbol-function 'sly-flex-completions) ; fuzzy completion
(setq sly-mrepl-history-file-name (expand-file-name "sly-mrepl-history" user-emacs-directory))
(setq sly-kill-without-query-p t) ; don't ask when killing REPL

;; Ensure history files exist to prevent read errors
(with-eval-after-load 'sly-mrepl
  (unless (file-exists-p sly-mrepl-history-file-name)
    (write-region "" nil sly-mrepl-history-file-name)))

;; Handle legacy sly-repl-history if it's referenced
(let ((legacy-history-file (expand-file-name "sly-repl-history" user-emacs-directory)))
  (unless (file-exists-p legacy-history-file)
    (write-region "" nil legacy-history-file)))

;; Quick evaluation keybindings for fast REPL-driven development
(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c C-e") 'sly-eval-last-expression)
  (define-key sly-mode-map (kbd "C-c C-r") 'sly-eval-region)
  (define-key sly-mode-map (kbd "C-c C-f") 'sly-eval-defun)
  (define-key sly-mode-map (kbd "C-c C-b") 'sly-eval-buffer)
  (define-key sly-mode-map (kbd "C-c C-p") 'sly-pprint-eval-last-expression)
  ;; Quick debugging with stickers
  (define-key sly-mode-map (kbd "C-c C-s") 'sly-stickers-dwim))

;; Enhanced REPL output for Advent of Code debugging
(defun aoc/setup-repl-output ()
  "Configure REPL for better AoC output visualization."
  (sly-eval '(cl:progn
              ;; Pretty print by default
              (cl:setf cl:*print-pretty* t)
              (cl:setf cl:*print-right-margin* 100)
              ;; Show more list elements
              (cl:setf cl:*print-length* 100)
              (cl:setf cl:*print-level* 10)
              ;; Useful for debugging sequences
              (cl:setf cl:*print-circle* t))))

(add-hook 'sly-connected-hook 'aoc/setup-repl-output)

;; Auto-complete
;; https://github.com/auto-complete
(use-package auto-complete
  :config
  (global-auto-complete-mode t)
  (add-to-list 'ac-modes 'sly-mrepl-mode))

(use-package ac-sly
  :after (auto-complete sly)
  :hook (sly-mode . set-up-sly-ac))

;; always split windows vertically
;; (I like the REPL on the right on most displays)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; colorful parenthesis matching
(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  :config
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#c66")  ; red
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#6c6")  ; green
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#69f")  ; blue
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#cc6")  ; yellow
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#6cc")  ; cyan
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#c6c")  ; magenta
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#ccc")  ; light gray
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#999")  ; medium gray
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#666")) ; dark gray

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Now you have a choice PAREDIT or LISPY - I go back and forth. Both
;; can be installed without too much redundancy.

;; Paredit for structured editing in Lisp source files only Note: This
;; is enabled ONLY for lisp-mode (source files), not for REPL or SLY
;; frames
;; http://danmidwood.com/content/2014/11/21/animated-paredit.html
(use-package paredit
  :hook (lisp-mode . enable-paredit-mode))

;; or use Lispy for a more vi single-key style
(use-package lispy
  :hook (lisp-mode . lispy-mode))

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Yasnippet for code templates (great for Advent of Code!)
(use-package yasnippet
  :custom
  (yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

;; Common yasnippet snippet collection
(use-package yasnippet-snippets
  :after yasnippet)

;; use w3m for hyperspec lookup (C-c C-d C-h)
(use-package w3m
  :config
  (defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
    (let ((browse-url-browser-function 'w3m-browse-url))
      (apply orig-fun args)))
  (advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m))
