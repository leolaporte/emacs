;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.

;; minor mode for Emacs that displays the key bindings following your currently entered incomplete command
(straight-use-package 'which-key)
(which-key-mode)

;; Hyperbole minor mode - M-Ret to click links https://www.gnu.org/software/hyperbole/
(straight-use-package 'hyperbole)
(hyperbole-mode 1)

;; Hyperbole will use the Org directory for rolodex etc.
(add-hook 'hyperbole-init-hook
	  (lambda ()
	    (require 'org)
	    (setq hyrolo-file-list (append (hyrolo-initialize-file-list)
					   (cddr (directory-files org-directory))))))

;; “switch windows with your shift key by pressing S-<left>, S-<right>, S-<up>, S-<down>.
;; (windmove-default-keybindings)  -- conflicts with org todo

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
(straight-use-package 'avy)
(avy-setup-default)
(global-set-key (kbd "C-c C-j") 'avy-resume)
(global-set-key (kbd "C-:") 'avy-goto-char)
(global-set-key (kbd "C-'") 'avy-goto-char-2)
(global-set-key (kbd "M-g w") 'avy-goto-word-1)
(global-set-key (kbd "M-j") 'avy-goto-char-timer)

;; a Collection of Ridiculously Useful eXtensions for Emacs
;; https://github.com/bbatsov/crux
;; see navigation.el for special crux keys
(straight-use-package 'crux)

;; CRUX key remaps
;; (global-set-key (kbd "C-c o") #'crux-open-with)

(global-set-key (kbd "s-r") #'crux-recentf-ido-find-file)

(global-set-key (kbd "C-k") #'crux-smart-kill-line)
(global-set-key (kbd "C-<backspace>") #'crux-kill-line-backwards)
(global-set-key (kbd "C-c f") #'crux-recentf-find-file)
(global-set-key (kbd "C-c e") #'crux-eval-and-replace) ; evaluate e-lisp exp and replace with result
(global-set-key (kbd "C-c I") #'crux-find-user-init-file)
(global-set-key (kbd "C-c d") #'crux-duplicate-current-line-or-region)
(global-set-key (kbd "C-c r") #'crux-rename-file-and-buffer)
(global-set-key (kbd "C-c k") #'crux-kill-other-buffers)

;; Crux remaps
(global-set-key [(shift return)] #'crux-smart-open-line)
(global-set-key [(control shift return)] #'crux-smart-open-line-above)
(global-set-key [remap move-beginning-of-line] #'crux-move-beginning-of-line)
(global-set-key [remap kill-whole-line] #'crux-kill-whole-line)

;; Increase the selected region by semantic units
;; https://github.com/magnars/expand-region.el
(straight-use-package 'expand-region)
;; bind to C-=
(require 'expand-region)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)

;; Turn on view mode in read-only buffers
(setq view-read-only t)
