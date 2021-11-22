;; Sets up shell integration
;; including some macOS specific mods
;; modified to use  vterm - Léo 2/24/21

;; my preferred terminal emulator package - requires emacs built with module support
(straight-use-package 'vterm)

;; set up system specific parameters
(if (eq system-type 'darwin) ; macOS
    (progn
      (straight-use-package 'exec-path-from-shell) ; imports $PATH  https://github.com/purcell/exec-path-from-shell
      (exec-path-from-shell-initialize)     
      (exec-path-from-shell-copy-envs '("PATH"))
      (setq explicit-shell-file-name "/opt/homebrew/bin/fish")
      (straight-use-package 'osx-lib))              ; mac specific utilities
  (setq explicit-shell-filename "/usr/bin/fish")) ; else Linux

(setq shell-file-name "fish")
(setq explicit-fish-args '("-l" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)

(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*vterm*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (vterm (getenv "SHELL")))
    (switch-to-buffer-other-window "*vterm*")))

(global-set-key (kbd "C-c t") #'visit-term-buffer)

