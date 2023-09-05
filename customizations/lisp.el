;; Lisp specific packages
;; including sly, paraedit

;; project navigation
;; projectile everywhere!
(straight-use-package 'projectile)
(projectile-global-mode)

;; Racket mode support
(straight-use-package 'racket-mode)
(setq racket-program "opt/homebrew/bin/racket")

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

;; load lispy for fast CL navigation
;; (straight-use-package 'lispy)
;; (add-hook 'sly-mode-hook (lambda () (lispy-mode 1)))

;; always split windows vertically
;; (I like the REPL on the right on most displays)
(setq split-height-threshold nil)
(setq split-width-threshold 0)

;; locate lisp package
(if (eq system-type 'darwin)
    (setq inferior-lisp-program "/opt/homebrew/bin/sbcl")  ;; MacOS
  (setq inferior-lisp-program "/usr/bin/sbcl"));; Linux

;; makes handling lisp expressions much, much easier
;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(straight-use-package 'smartparens)
(straight-use-package 'hydra) ; for keystroke hydra below

;; SmartParent config from its author
;; https://github.com/Fuco1/.emacs.d/blob/master/files/smartparens.el

;; -*- lexical-binding: t -*-
;;;;;;;;;
;; global
(require 'smartparens-config)

(add-hook 'eval-expression-minibuffer-setup-hook 'turn-on-smartparens-strict-mode)

;;;;;;;;;;;;;;;;;;;;;;;;
;; keybinding management
(define-key smartparens-mode-map (kbd "C-M-f") 'sp-forward-sexp)
(define-key smartparens-mode-map (kbd "C-M-b") 'sp-backward-sexp)

(define-key smartparens-mode-map (kbd "C-M-d") 'sp-down-sexp)
(define-key smartparens-mode-map (kbd "C-M-a") 'sp-backward-down-sexp)
(define-key smartparens-mode-map (kbd "C-S-d") 'sp-beginning-of-sexp)
(define-key smartparens-mode-map (kbd "C-S-a") 'sp-end-of-sexp)

(define-key smartparens-mode-map (kbd "C-M-e") 'sp-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-u") 'sp-backward-up-sexp)
(define-key smartparens-mode-map (kbd "C-M-t") 'sp-transpose-sexp)

(define-key smartparens-mode-map (kbd "C-M-n") 'sp-forward-hybrid-sexp)
(define-key smartparens-mode-map (kbd "C-M-p") 'sp-backward-hybrid-sexp)

(define-key smartparens-mode-map (kbd "C-M-k") 'sp-kill-sexp)
(define-key smartparens-mode-map (kbd "C-M-w") 'sp-copy-sexp)

(define-key smartparens-mode-map (kbd "M-<delete>") 'sp-unwrap-sexp)
(define-key smartparens-mode-map (kbd "M-<backspace>") 'sp-backward-unwrap-sexp)

(define-key smartparens-mode-map (kbd "C-<right>") 'sp-forward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-<left>") 'sp-forward-barf-sexp)
(define-key smartparens-mode-map (kbd "C-M-<left>") 'sp-backward-slurp-sexp)
(define-key smartparens-mode-map (kbd "C-M-<right>") 'sp-backward-barf-sexp)

(define-key smartparens-mode-map (kbd "M-D") 'sp-splice-sexp)
(define-key smartparens-mode-map (kbd "C-M-<delete>") 'sp-splice-sexp-killing-forward)
(define-key smartparens-mode-map (kbd "C-M-<backspace>") 'sp-splice-sexp-killing-backward)
(define-key smartparens-mode-map (kbd "C-S-<backspace>") 'sp-splice-sexp-killing-around)

(define-key smartparens-mode-map (kbd "C-]") 'sp-select-next-thing-exchange)
(define-key smartparens-mode-map (kbd "C-<left_bracket>") 'sp-select-previous-thing)
(define-key smartparens-mode-map (kbd "C-M-]") 'sp-select-next-thing)

(define-key smartparens-mode-map (kbd "M-F") 'sp-forward-symbol)
(define-key smartparens-mode-map (kbd "M-B") 'sp-backward-symbol)

(define-key smartparens-mode-map (kbd "C-\"") 'sp-change-inner)
(define-key smartparens-mode-map (kbd "M-i") 'sp-change-enclosing)

(bind-key "C-c f" (lambda () (interactive) (sp-beginning-of-sexp 2)) smartparens-mode-map)
(bind-key "C-c b" (lambda () (interactive) (sp-beginning-of-sexp -2)) smartparens-mode-map)

(bind-key "C-M-s"
          (defhydra smartparens-hydra ()
                    "Smartparens"
                    ("d" sp-down-sexp "Down")
                    ("e" sp-up-sexp "Up")
                    ("u" sp-backward-up-sexp "Up")
                    ("a" sp-backward-down-sexp "Down")
                    ("f" sp-forward-sexp "Forward")
                    ("b" sp-backward-sexp "Backward")
                    ("k" sp-kill-sexp "Kill" :color blue)
                    ("q" nil "Quit" :color blue))
          smartparens-mode-map)

(bind-key "H-t" 'sp-prefix-tag-object smartparens-mode-map)
(bind-key "H-p" 'sp-prefix-pair-object smartparens-mode-map)
(bind-key "H-y" 'sp-prefix-symbol-object smartparens-mode-map)
(bind-key "H-h" 'sp-highlight-current-sexp smartparens-mode-map)
(bind-key "H-e" 'sp-prefix-save-excursion smartparens-mode-map)
(bind-key "H-s c" 'sp-convolute-sexp smartparens-mode-map)
(bind-key "H-s a" 'sp-absorb-sexp smartparens-mode-map)
(bind-key "H-s e" 'sp-emit-sexp smartparens-mode-map)
(bind-key "H-s p" 'sp-add-to-previous-sexp smartparens-mode-map)
(bind-key "H-s n" 'sp-add-to-next-sexp smartparens-mode-map)
(bind-key "H-s j" 'sp-join-sexp smartparens-mode-map)
(bind-key "H-s s" 'sp-split-sexp smartparens-mode-map)
(bind-key "H-s r" 'sp-rewrap-sexp smartparens-mode-map)
(defvar hyp-s-x-map)
(define-prefix-command 'hyp-s-x-map)
(bind-key "H-s x" hyp-s-x-map smartparens-mode-map)
(bind-key "H-s x x" 'sp-extract-before-sexp smartparens-mode-map)
(bind-key "H-s x a" 'sp-extract-after-sexp smartparens-mode-map)
(bind-key "H-s x s" 'sp-swap-enclosing-sexp smartparens-mode-map)

(bind-key "C-x C-t" 'sp-transpose-hybrid-sexp smartparens-mode-map)

(bind-key ";" 'sp-comment emacs-lisp-mode-map)

(bind-key [remap c-electric-backspace] 'sp-backward-delete-char smartparens-strict-mode-map)

;;;;;;;;;;;;;;;;;;
;; pair management

(sp-with-modes '(minibuffer-mode)
  (sp-local-pair "'" nil :actions nil)
  (sp-local-pair "(" nil :wrap "C-("))

(sp-with-modes 'org-mode
  (sp-local-pair "=" "=" :wrap "C-="))

(sp-with-modes 'textile-mode
  (sp-local-pair "*" "*")
  (sp-local-pair "_" "_")
  (sp-local-pair "@" "@"))

(sp-with-modes 'web-mode
  (sp-local-pair "{{#if" "{{/if")
  (sp-local-pair "{{#unless" "{{/unless"))

;;; tex-mode latex-mode
(sp-with-modes '(tex-mode plain-tex-mode latex-mode)
  (sp-local-tag "i" "\"<" "\">"))

;;; lisp modes
(sp-with-modes sp--lisp-modes
  (sp-local-pair "(" nil
                 :wrap "C-("
                 :pre-handlers '(my-add-space-before-sexp-insertion)
                 :post-handlers '(my-add-space-after-sexp-insertion)))

(defun my-add-space-after-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (forward-char (sp-get-pair id :cl-l))
      (when (or (eq (char-syntax (following-char)) ?w)
                (looking-at (sp--get-opening-regexp)))
        (insert " ")))))

(defun my-add-space-before-sexp-insertion (id action _context)
  (when (eq action 'insert)
    (save-excursion
      (backward-char (length id))
      (when (or (eq (char-syntax (preceding-char)) ?w)
                (and (looking-back (sp--get-closing-regexp))
                     (not (eq (char-syntax (preceding-char)) ?'))))
        (insert " ")))))


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

;; Automatic pair matching
(setq electric-pair-pairs '((?\{ . ?\})
			    (?\( . ?\))
			    (?\[ . ?\])
			    (?\" . ?\")))
(electric-pair-mode t)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

;;;; Bits from Mark Triggs's .emacs
;;;; Just a couple of things I find useful for Lisp editing.
;;;; https://www.cliki.net/Bits%20from%20Mark%20Triggs%27s%20.emacs

;; browser for display of hyperspec pages
(straight-use-package 'w3m) ; w3m must be separately installed on system
(require 'w3m)

(defadvice common-lisp-hyperspec (around hyperspec-lookup-w3m () activate)
  "Browse the Common Lisp HyperSpec using w3m.
When leaving w3m, restore the original window configuration."
  (let* ((window-configuration (current-window-configuration))
         (browse-url-browser-function
          `(lambda (url new-window)
             (unless (member (current-buffer) (w3m-list-buffers))
               (select-window (split-window-vertically)))
             (w3m-browse-url url nil)
             (let ((hs-map (copy-keymap w3m-mode-map)))
               (define-key hs-map (kbd "q")
                 (lambda ()
                   (interactive)
                   (kill-buffer nil)
                   (set-window-configuration ,window-configuration)))
               (use-local-map hs-map)))))
    ad-do-it))

(defun hyperspec-lookup--hyperspec-lookup-w3m (orig-fun &rest args)
  (let ((browse-url-browser-function 'w3m-browse-url))
    (apply orig-fun args)))

(advice-add 'hyperspec-lookup :around #'hyperspec-lookup--hyperspec-lookup-w3m)

(defun lisp-reindent-defun ()
  "Indent the current defun."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    (indent-sexp)))
