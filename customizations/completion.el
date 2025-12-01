;; -*- lexical-binding: t; -*-

;;; Replacing Helm with  Vertico, Embark, Consult, Orderless and Marginalia
;;; via
;;; https://codeberg.org/vifon/emacs-config/src/branch/master/emacs.d/lisp/20-completing-read.eln
;;; Leo Laporte 6 Sept 2022

(use-package vertico
  :init
  (vertico-mode 1)
  (vertico-mouse-mode 1)
  (vertico-multiform-mode 1)
  :config
  (add-hook 'minibuffer-setup-hook #'vertico-repeat-save)
  (setq vertico-multiform-categories '((consult-grep buffer))
        vertico-multiform-commands '((tmm-menubar flat)
                                     (tmm-shortcut flat)))
  :bind (:map global-map
              ("C-x M-r" . vertico-repeat)
              :map vertico-map
              ("C-l" . vertico-directory-delete-word)
              ("M-g" . vertico-multiform-grid)
              ("M-q" . vertico-multiform-flat)))

(use-package vertico-posframe
  :if (display-graphic-p)
  :after vertico
  :config
  (setq vertico-posframe-poshandler #'posframe-poshandler-frame-center
        vertico-posframe-width 150
        vertico-posframe-height vertico-count)
  (add-hook 'after-init-hook #'vertico-posframe-mode))

(use-package orderless
  :custom
  (orderless-matching-styles '(orderless-regexp
                                orderless-initialism
                                orderless-prefixes))
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  (completion-styles '(orderless))
  :config
  (defun vifon/orderless-without-if-bang (pattern index total)
    (when (string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1))))
  (defun vifon/orderless-literal-if-equal (pattern index total)
    (when (string-suffix-p "=" pattern)
      `(orderless-literal . ,(substring pattern 0 -1))))
  (setq orderless-style-dispatchers '(vifon/orderless-without-if-bang
                                      vifon/orderless-literal-if-equal)))

(use-package embark
  :bind (("C-c o" . embark-act)
         ("C-." . embark-act)
         :map minibuffer-local-map
         ("M-o" . embark-act))
  :custom
  (embark-mixed-indicator-delay 2)
  :config
  ;; Unbind the dangerous `global-set-key' and `local-set-key' actions
  (define-key embark-command-map (kbd "g") nil)
  (define-key embark-command-map (kbd "l") nil)

  ;; Make the eval action editable
  (cl-pushnew 'embark--allow-edit
              (alist-get 'pp-eval-expression embark-target-injection-hooks))

  ;; Reload the project list after using C-u `embark-act' with `project-forget-project'
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
  (define-key embark-file-map (kbd "a") #'embark-attach-file))

(use-package embark-consult
  :after (embark consult))

(use-package marginalia
  :init
  (marginalia-mode 1)
  :bind (:map minibuffer-local-map
              ("C-o" . marginalia-cycle)))

(use-package consult
  :bind (("M-s f" . consult-line)
         ("M-g g" . consult-line)
         ("M-g o" . consult-outline)
         ("M-g i" . consult-imenu)
         ("M-g r" . consult-ripgrep)
         ("C-x C-r" . consult-recent-file)
         ([remap switch-to-buffer] . consult-buffer)
         ([remap yank-pop] . consult-yank-pop)
         ([remap goto-line] . consult-goto-line)
         :map minibuffer-local-map
         ([remap previous-matching-history-element] . consult-history)
         :map isearch-mode-map
         ("TAB" . vifon/isearch-to-consult-line))
  :custom
  (consult-project-root-function #'vc-root-dir)
  :config
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

  ;; Disable consult-buffer project-related capabilities as they are very slow in TRAMP
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
      (consult-line query))))

(use-package corfu
  :custom
  (corfu-auto t)                 ;; Enable auto completion
  (corfu-auto-delay 0.1)         ;; Delay before showing popup (seconds)
  (corfu-auto-prefix 2)          ;; Minimum prefix length to trigger
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect 'prompt)      ;; Preselect the prompt
  :init
  (global-corfu-mode 1))

;; Cape provides additional completion-at-point backends
(use-package cape
  :config
  ;; Add useful defaults
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

;; Company mode - fallback completion for contexts where Corfu doesn't work
(use-package company
  :hook (sly-mrepl-mode . company-mode)
  :custom
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  :config
  ;; Don't interfere with Corfu in regular buffers
  (setq company-global-modes '(not prog-mode text-mode)))

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
