;; These customizations change the way emacs looks and disable/enable
;; some user interface elements.
;; Léo Laporte Sun Nov 21 10:30:29 2021

;; System specific window and font sizes
(cond ((equal system-name "MBP-14")
       (set-face-attribute 'default nil :family "iosevka nerd font mono" :height 180)
       (setq initial-frame-alist '((top . 10) (left . 10) (width . 120) (height . 40))))

      ((equal system-name "mojo-ryzen")        
       (set-face-attribute 'default nil :family "Iosevka" :height 140) 
       (setq initial-frame-alist '((top . 150) (left . 800) (width . 150) (height . 50))))

      ((equal system-name "xps13")        
       (set-face-attribute 'default nil :family "Iosevka" :height 140) 
       (setq initial-frame-alist '((top . 10) (left . 10) (width . 100) (height . 30))))

      ((equal system-name "oryx-popos")        
       (set-face-attribute 'default nil :family "Menlo" :height 140) 
       (setq initial-frame-alist '((top . 10) (left . 10) (width . 100) (height . 40))))

      ((equal system-name "darterpro")        
       (set-face-attribute 'default nil :family "ttf-iosevka" :height 140) 
       (setq initial-frame-alist '((top . 40) (left . 20) (width . 120) (height . 30))))

      ((equal system-name "framework")
       (set-face-attribute 'default nil :family "Iosevka" :height 140) 
       (setq initial-frame-alist '((top . 20) (left . 20) (width . 100) (height . 40)))))

;; Add Full screen toggle
(defun toggle-fullscreen ()
  "Toggle full screen"
  (interactive)
  (set-frame-parameter
   nil 'fullscreen
   (when (not (frame-parameter nil 'fullscreen)) 'fullboth)))

;; Turn off the menu bar at the top of each frame because it's distracting
(menu-bar-mode -1)

;; Show line numbers
;; (global-linum-mode)      ; in gutter
(setf column-number-mode t) ; in mode line
(setf line-number-mode t)   ; in mode line

;; Remove graphical tool bar
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))

;; Don't show native OS scroll bars for buffers
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; remember window layouts with winner-mode
(winner-mode 1)

;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes")
(add-to-list 'load-path "~/.emacs.d/themes")
(load-theme 'tomorrow-night-bright t)

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
 select-enable-clipboard t

 ;; automatically copy the primary selection (if supported by the os)
 select-enable-primary t
 
 ;; Save clipboard strings into kill ring before replacing them.
 ;; When one selects something in another program to paste it into Emacs,
 ;; but kills something in Emacs before actually pasting it,
 ;; this selection is gone unless this variable is non-nil
 save-interprogram-paste-before-kill t

 ;; Shows all options when running apropos. For more info,
 ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
 apropos-do-all t

 ;; Mouse yank commands yank at point instead of at click.
 mouse-yank-at-point t

 ;; copy mouse selected region automatically
 mouse-drag-copy-region t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

