;; These customizations change the way emacs looks and disable/enable
;; some user interface elements.
;; Léo Laporte Sun Nov 21 10:30:29 2021

;; System specific window and font sizes
;; specify system name with (system-name) - eval with Meta-:
(cond ((or (equal system-name "MBP-14.local") (equal system-name "MBP-14"))
       ;; M-x toggle-frame-fullscreen (below MacBook notch)
       (global-set-key (kbd "A-<f10>") 'toggle-frame-fullscreen)
       ;; M-x toggle-frame-maximized (M-<F10>) (all the way over notch)
       (set-face-attribute 'default nil :family "Fira Code Retina" :height 180)
       (setq initial-frame-alist '((top . 20) (left . 15) (width . 100) (height . 40))))

      ((equal system-name "mojo-ryzen")
       (set-face-attribute 'default nil :family "Iosevka" :height 140)
       (setq initial-frame-alist '((top . 150) (left . 1000) (width . 150) (height . 75))))

      ((or (equal system-name "max-mac.local") (equal system-name "max-mac"))
       (set-face-attribute 'default nil :family "Fira Code Retina" :height 180)
       (setq initial-frame-alist '((top . 100) (left . 500) (width . 250) (height . 85))))

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

;;; Modus Operandi high contrast theme - built-into Emacs >28.1
;;; https://protesilaos.com/emacs/modus-themes

;; Add customizations prior to loading the themes
(setq modus-themes-italic-constructs t
      modus-themes-bold-constructs t
      modus-themes-mixed-fonts t
      modus-themes-subtle-line-numbers t
      modus-themes-intense-mouseovers t
      modus-themes-deuteranopia t
      modus-themes-tabs-accented t
      modus-themes-variable-pitch-ui t
      modus-themes-inhibit-reload t ; only applies to `customize-set-variable' and related

      modus-themes-fringes nil ; {nil,'subtle,'intense}

      ;; Options for `modus-themes-lang-checkers' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `straight-underline', `text-also', `background',
      ;; `intense' OR `faint'.
      modus-themes-lang-checkers nil

      ;; Options for `modus-themes-mode-line' are either nil, or a list
      ;; that can combine any of `3d' OR `moody', `borderless',
      ;; `accented', a natural number for extra padding (or a cons cell
      ;; of padding and NATNUM), and a floating point for the height of
      ;; the text relative to the base font size (or a cons cell of
      ;; height and FLOAT)
      modus-themes-mode-line '(accented borderless (padding . 4) (height . 0.9))

      ;; Same as above:
      ;; modus-themes-mode-line '(accented borderless 4 0.9)

      ;; Options for `modus-themes-markup' are either nil, or a list
      ;; that can combine any of `bold', `italic', `background',
      ;; `intense'.
      modus-themes-markup '(background italic)

      ;; Options for `modus-themes-syntax' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `faint', `yellow-comments', `green-strings', `alt-syntax'
      modus-themes-syntax '(yellow-comments green-strings alt-syntax)

      ;; Options for `modus-themes-hl-line' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `accented', `underline', `intense'
      modus-themes-hl-line '(underline accented)

      ;; Options for `modus-themes-paren-match' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `bold', `intense', `underline'
      modus-themes-paren-match '(bold intense)

      ;; Options for `modus-themes-links' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `neutral-underline' OR `no-underline', `faint' OR `no-color',
      ;; `bold', `italic', `background'
      modus-themes-links '(neutral-underline background)

      ;; Options for `modus-themes-box-buttons' are either nil (the
      ;; default), or a list that can combine any of `flat', `accented',
      ;; `faint', `variable-pitch', `underline', `all-buttons', the
      ;; symbol of any font weight as listed in `modus-themes-weights',
      ;; and a floating point number (e.g. 0.9) for the height of the
      ;; button's text.
      modus-themes-box-buttons '(variable-pitch flat faint 0.9)

      ;; Options for `modus-themes-prompts' are either nil (the
      ;; default), or a list of properties that may include any of those
      ;; symbols: `background', `bold', `gray', `intense', `italic'
      modus-themes-prompts '(intense bold)

      ;; The `modus-themes-completions' is an alist that reads three
      ;; keys: `matches', `selection', `popup'.  Each accepts a nil
      ;; value (or empty list) or a list of properties that can include
      ;; any of the following (for WEIGHT read further below):
      ;;
      ;; `matches' - `background', `intense', `underline', `italic', WEIGHT
      ;; `selection' - `accented', `intense', `underline', `italic', `text-also' WEIGHT
      ;; `popup' - same as `selected'
      ;; `t' - applies to any key not explicitly referenced (check docs)
      ;;
      ;; WEIGHT is a symbol such as `semibold', `light', or anything
      ;; covered in `modus-themes-weights'.  Bold is used in the absence
      ;; of an explicit WEIGHT.
      modus-themes-completions '((matches . (extrabold))
                                 (selection . (semibold accented))
                                 (popup . (accented intense)))

      modus-themes-mail-citations nil ; {nil,'intense,'faint,'monochrome}

      ;; Options for `modus-themes-region' are either nil (the default),
      ;; or a list of properties that may include any of those symbols:
      ;; `no-extend', `bg-only', `accented'
      modus-themes-region '(bg-only no-extend)

      ;; Options for `modus-themes-diffs': nil, 'desaturated, 'bg-only
      modus-themes-diffs 'desaturated

      modus-themes-org-blocks 'gray-background ; {nil,'gray-background,'tinted-background}

      modus-themes-org-agenda ; this is an alist: read the manual or its doc string
      '((header-block . (variable-pitch 1.3))
        (header-date . (grayscale workaholic bold-today 1.1))
        (event . (accented varied))
        (scheduled . uniform)
        (habit . traffic-light))

      modus-themes-headings ; this is an alist: read the manual or its doc string
      '((1 . (overline background variable-pitch 1.3))
        (2 . (rainbow overline 1.1))
        (t . (semibold))))

(load-theme 'modus-vivendi)
(define-key global-map (kbd "<f5>") #'modus-themes-toggle)

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

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") #'(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)
