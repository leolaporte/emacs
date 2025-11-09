;; Lisp specific packages
;; including sly and lispy
;; Leo Laporte 5 Sept 2023
;; see .emacs.d/keybindings.md for custom keybindings

;; First locate lisp package depending on OS
(if (eq system-type 'darwin)                              ; macOS
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl") ;; MacOS
  (setq inferior-lisp-program "/usr/bin/sbcl")) ;; otherwise Linux

;; project navigation
;; projectile everywhere!
(straight-use-package 'projectile)
(require 'projectile)
(projectile-global-mode)
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(straight-use-package 'treemacs)
(require 'treemacs)

(straight-use-package 'treemacs-projectile)
(require 'treemacs-projectile)

;; Common Lisp support - btw I use sly
(straight-use-package 'sly)
(require 'sly)

;; Sly extensions for enhanced Common Lisp development
(straight-use-package 'sly-quicklisp)
(with-eval-after-load 'sly
  (require 'sly-quicklisp))

(straight-use-package 'sly-asdf)
(with-eval-after-load 'sly
  (require 'sly-asdf))

(straight-use-package 'sly-macrostep)
(with-eval-after-load 'sly
  (require 'sly-macrostep))

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
(straight-use-package 'auto-complete)
(with-eval-after-load 'sly
  (require 'auto-complete))

(straight-use-package 'ac-sly)  ; add support for Sly
(with-eval-after-load 'sly
  (require 'ac-sly))

(add-hook 'sly-mode-hook 'set-up-sly-ac)

(eval-after-load 'auto-complete
  '(add-to-list 'ac-modes 'sly-mrepl-mode))

(global-auto-complete-mode t)

;; always split windows vertically
;; (I like the REPL on the right on most displays)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; colorful parenthesis matching
(straight-use-package 'rainbow-delimiters)
(require 'rainbow-delimiters)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
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

;; Paredit for structured editing in Lisp source files only Note: This
;; is enabled ONLY for lisp-mode (source files), not for REPL or SLY
;; frames
(straight-use-package 'paredit)
(require 'paredit)
(add-hook 'lisp-mode-hook #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;; Yasnippet for code templates (great for Advent of Code!)
(straight-use-package 'yasnippet)
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode 1)

;; Common yasnippet snippet collection
(straight-use-package 'yasnippet-snippets)
(with-eval-after-load 'yasnippet
  (require 'yasnippet-snippets))

;; use w3m for hyperspec lookup (C-c C-d C-h)
(straight-use-package 'emacs-w3m)
(defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
  (let ((browse-url-browser-function 'w3m-browse-url))
    (apply orig-fun args)))

(advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m)
