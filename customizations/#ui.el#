;; These customizations change the way emacs looks and disable/enable
;; some user interface elements.
;; Léo Laporte Sun Nov 21 10:30:29 2021

;; System specific window and font sizes
;; specify system name with (system-name) - eval with Meta-:
(cond ((equal system-name "MBP-14.local")
       ;; M-x toggle-frame-fullscreen (below MacBook notch)
       (global-set-key (kbd "A-<f10>") 'toggle-frame-fullscreen)
       ;; M-x toggle-frame-maximized (M-<F10>) (all the way over notch)
       (set-face-attribute 'default nil :family "Fira Code Retina" :height 180)
       (setq initial-frame-alist '((top . 20) (left . 15) (width . 100) (height . 40))))

      ((equal system-name "mojo-ryzen")        
       (set-face-attribute 'default nil :family "Iosevka" :height 140) 
       (setq initial-frame-alist '((top . 150) (left . 1000) (width . 150) (height . 75))))

      ((equal system-name "max-mac.local")           
       (set-face-attribute 'default nil :family "Fira Code Retina" :height 180) 
       (setq initial-frame-alist '((top . 100) (left . 800) (width . 250) (height . 90))))
      
      ((equal system-name "xps13")        
       (set-face-attribute 'default nil :family "Iosevka" :height 140) 
       (setq initial-frame-alist '((top . 10) (left . 10) (width . 100) (height . 30))))

      ((equal system-name "oryx")        
       (set-face-attribute 'default nil :family "Menlo" :height 140)  
       (setq initial-frame-alist '((top . 10) (left . 10) (width . 120) (height . 50))))

      ((equal system-name "darterpro")        
       (set-face-attribute 'default nil :family "ttf-iosevka" :height 140) 
       (setq initial-frame-alist '((top . 40) (left . 20) (width . 130) (height . 45))))

      ((equal system-name "studio-lenovo")        
       (set-face-attribute 'default nil :family "ttf-iosevka" :height 140) 
       (setq initial-frame-alist '((top . 50) (left . 80) (width . 100) (height . 40))))

      ((equal system-name "framework")
       (set-face-attribute 'default nil :family "Iosevka" :height 140) 
       (setq initial-frame-alist '((top . 20) (left . 20) (width . 100) (height . 40)))))

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

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") #'(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)

