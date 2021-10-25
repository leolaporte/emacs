;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
;; modified to use  vterm - Leo 2/24/21

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

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

;; NOTE: change next line to reflect location of fish shell
(setq explicit-shell-file-name "/opt/homebrew/bin/fish")
(setq shell-file-name "fish")
(setq explicit-fish-args '("-l" "-i"))
(setenv "SHELL" shell-file-name)
(add-hook 'comint-output-filter-functions 'comint-strip-ctrl-m)
