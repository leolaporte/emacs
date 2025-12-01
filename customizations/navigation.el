;; -*- lexical-binding: t; -*-

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; minor mode for Emacs that displays the key bindings following your currently entered incomplete command
(use-package which-key
  :config
  (which-key-mode))

;; "switch windows with your shift key by pressing S-<left>, S-<right>, S-<up>, S-<down>.
(windmove-default-keybindings) ; unfortunately conflicts with org todo

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)

;; Use avy-mode to jump to words in buffer
(use-package avy
  :config
  (avy-setup-default)
  :bind (("C-c C-j" . avy-resume)
         ("C-:" . avy-goto-char)
         ("C-'" . avy-goto-char-2)
         ("M-g w" . avy-goto-word-1)
         ("M-j" . avy-goto-char-timer)))

;; a Collection of Ridiculously Useful eXtensions for Emacs
;; https://github.com/bbatsov/crux
;; see navigation.el for special crux keys
(use-package crux
  :bind (("s-r" . crux-recentf-ido-find-file)
         ("C-k" . crux-smart-kill-line)
         ("C-<backspace>" . crux-kill-line-backwards)
         ("C-c f" . crux-recentf-find-file)
         ("C-c C-e" . crux-eval-and-replace)  ;; Changed from C-c e to free up prefix for EC
         ("C-c I" . crux-find-user-init-file)
         ("C-c d" . crux-duplicate-current-line-or-region)
         ("C-c r" . crux-rename-file-and-buffer)
         ("C-c k" . crux-kill-other-buffers)
         ("<S-return>" . crux-smart-open-line)
         ("<C-S-return>" . crux-smart-open-line-above)
         ([remap move-beginning-of-line] . crux-move-beginning-of-line)
         ([remap kill-whole-line] . crux-kill-whole-line)))

;; Increase the selected region by semantic units
;; https://github.com/magnars/expand-region.el
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Turn on view mode in read-only buffers
(setq view-read-only t)
