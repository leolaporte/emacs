;; -*- lexical-binding: t; -*-

;;; Replacing Helm with  Vertico, Embark, Consult, Orderless and Marginalia
;;; via
;;; https://codeberg.org/vifon/emacs-config/src/branch/master/emacs.d/lisp/20-completing-read.eln
;;; Leo Laporte 6 Sept 2022



(straight-use-package '(vertico :files (:defaults "extensions/*")))
(require 'vertico)
(vertico-mode 1)
(define-key global-map (kbd "C-x M-r") 'vertico-repeat)
(define-key vertico-map (kbd "C-l") 'vertico-directory-delete-word)
(define-key vertico-map (kbd "M-g") 'vertico-multiform-grid)
(define-key vertico-map (kbd "M-q") 'vertico-multiform-flat)
(add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
(vertico-mouse-mode 1)
(vertico-multiform-mode 1)
(setq vertico-multiform-categories '((consult-grep buffer))
      vertico-multiform-commands '((tmm-menubar flat)
                                   (tmm-shortcut flat)))
(when (display-graphic-p)
  (straight-use-package 'vertico-posframe)
  (require 'vertico-posframe)
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center)
  (setq vertico-posframe-width 150)
  (setq vertico-posframe-height vertico-count)
  (add-hook 'after-init-hook #'vertico-posframe-mode))

(straight-use-package 'orderless)
(require 'orderless)
(setq orderless-matching-styles '(orderless-regexp
                                  orderless-initialism
                                  orderless-prefixes)
      orderless-component-separator #'orderless-escapable-split-on-space)

;; Use the built-in "partial-completion" style to complete
;; file inputs such as "/e/ni/co.nix" into
;; "/etc/nixos/configuration.nix".  The "basic" style is
;; needed to support the hostname completion in the TRAMP
;; inputs such as "/sshx:HOSTNAME".
(setq completion-category-defaults nil
      completion-category-overrides '((file (styles basic partial-completion))))

(setq completion-styles '(orderless))

(defun vifon/orderless-without-if-bang (pattern index total)
  (when (string-prefix-p "!" pattern)
    `(orderless-without-literal . ,(substring pattern 1))))
(defun vifon/orderless-literal-if-equal (pattern index total)
  (when (string-suffix-p "=" pattern)
    `(orderless-literal . ,(substring pattern 0 -1))))
(setq orderless-style-dispatchers '(vifon/orderless-without-if-bang
                                    vifon/orderless-literal-if-equal))

(straight-use-package 'embark)
(require 'embark)
(global-set-key (kbd "C-c o") 'embark-act)
(global-set-key (kbd "C-.") 'embark-act)
(define-key minibuffer-local-map (kbd "M-o") 'embark-act)
;; Unbind the dangerous `global-set-key' and `local-set-key'
;; actions.  It's far too easy to accidentally bind over some
;; `self-insert-command' binding or even over
;; \\[keyboard-quit].
(define-key embark-command-map (kbd "g") nil)
(define-key embark-command-map (kbd "l") nil)
(setq embark-mixed-indicator-delay 2)

;; Make the eval action editable.  Evaluating code
;; in-place is simple enough without Embark, if I invoke
;; it with Embark, I almost definitely want to edit the
;; expression beforehand.  And even if not, I can
;; just confirm.
(cl-pushnew 'embark--allow-edit
            (alist-get 'pp-eval-expression embark-target-injection-hooks))

;; Reload the project list after using
;; C-u `embark-act' with `project-forget-project'.
(cl-pushnew 'embark--restart
            (alist-get 'project-forget-project embark-post-action-hooks))

(defun embark-act-with-eval (expression)
  "Evaluate EXPRESSION and call `embark-act' on the result."
  (interactive "sExpression: ")
  (with-temp-buffer
    (let ((expr-value (eval (read expression))))
      (insert (if (stringp expr-value)
                  expr-value
                (format "%S" expr-value))))
    (embark-act)))

(dolist (keymap (list embark-variable-map embark-expression-map))
  (define-key keymap (kbd "v") #'embark-act-with-eval))

;; Source: https://github.com/oantolin/embark/wiki/Additional-Actions#attaching-file-to-an-email-message
(autoload 'gnus-dired-attach "gnus-dired" nil t)
(defun embark-attach-file (file)
  "Attach FILE to an email message."
  (interactive "fAttach: ")
  (gnus-dired-attach (list file)))
(define-key embark-file-map (kbd "a") #'embark-attach-file)

(straight-use-package 'embark-consult)
(with-eval-after-load 'embark
  (with-eval-after-load 'consult
    (require 'embark-consult)))

(straight-use-package 'marginalia)
(require 'marginalia)
(define-key minibuffer-local-map (kbd "C-o") 'marginalia-cycle)
(marginalia-mode 1)

(straight-use-package 'consult)
(require 'consult)
(global-set-key (kbd "M-s f") 'consult-line)
(global-set-key (kbd "M-g g") 'consult-line)
(global-set-key (kbd "M-g o") 'consult-outline)
(global-set-key (kbd "M-g i") 'consult-imenu)
(global-set-key (kbd "M-g r") 'consult-ripgrep)
(global-set-key (kbd "C-x C-r") 'consult-recent-file)
(global-set-key [remap switch-to-buffer] 'consult-buffer)
(global-set-key [remap yank-pop] 'consult-yank-pop)
(global-set-key [remap goto-line] 'consult-goto-line)
(define-key minibuffer-local-map [remap previous-matching-history-element] 'consult-history)
(define-key isearch-mode-map (kbd "TAB") 'vifon/isearch-to-consult-line)

(setq consult-project-root-function #'vc-root-dir)
(consult-customize
 consult-ripgrep consult-grep
 consult-buffer consult-recent-file
 :preview-key (kbd "M-."))

(defun vifon/orderless-fix-consult-tofu (pattern index total)
  "Ignore the last character which is hidden and used only internally."
  (when (string-suffix-p "$" pattern)
    `(orderless-regexp . ,(concat (substring pattern 0 -1)
                                  "[\x200000-\x300000]*$"))))

(dolist (command '(consult-buffer consult-line))
  (advice-add command :around
              (lambda (orig &rest args)
                (let ((orderless-style-dispatchers (cons #'vifon/orderless-fix-consult-tofu
                                                         orderless-style-dispatchers)))
                  (apply orig args)))))

;; Disable consult-buffer project-related capabilities as
;; they are very slow in TRAMP.
(setq consult-buffer-sources
      (delq 'consult--source-project-buffer
            (delq 'consult--source-project-file consult-buffer-sources)))

(setq consult--source-hidden-buffer
      (plist-put consult--source-hidden-buffer :narrow ?h))

(defun vifon/isearch-to-consult-line ()
  "Search using `consult-line' what was being searched with `isearch'."
  (interactive)
  (isearch-exit)
  (let ((query (if isearch-regexp
                   isearch-string
                 (regexp-quote isearch-string))))
    (consult-line query)))

(straight-use-package 'corfu)
(require 'corfu)
(global-corfu-mode 1)

;; Cape provides additional completion-at-point backends
(straight-use-package 'cape)
(require 'cape)
;; Add useful defaults
(add-to-list 'completion-at-point-functions #'cape-dabbrev)
(add-to-list 'completion-at-point-functions #'cape-file)

;;; https://with-emacs.com/posts/tutorials/customize-completion-at-point/
(autoload 'ffap-file-at-point "ffap")
(add-hook 'completion-at-point-functions
          (defun complete-path-at-point+ ()
            (let ((fn (ffap-file-at-point))
                  (fap (thing-at-point 'filename)))
              (when (and (or fn (equal "/" fap))
                         (save-excursion
                           (search-backward fap (line-beginning-position) t)))
                (list (match-beginning 0)
                      (match-end 0)
                      #'completion-file-name-table :exclusive 'no))))
          'append)

;;; Add prompt indicator to `completing-read-multiple'.
;;; We display [CRM<separator>], e.g., [CRM,] if the separator is a comma.
;;;
;;; Taken from the Vertico docs.
(defun crm-indicator (args)
  (cons (format "[CRM%s] %s"
                (replace-regexp-in-string
                 "\\`\\[.*?]\\*\\|\\[.*?]\\*\\'" ""
                 crm-separator)
                (car args))
        (cdr args)))
(advice-add #'completing-read-multiple :filter-args #'crm-indicator)

(setq enable-recursive-minibuffers t)
(minibuffer-depth-indicate-mode 1)

;;; Use the completing-read UI for the M-tab completion unless
;;; overridden (for example by `corfu').
(setq-default completion-in-region-function
              (lambda (&rest args)
                (apply (if vertico-mode
                           #'consult-completion-in-region
                         #'completion--in-region)
                       args)))
