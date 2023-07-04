;;; ac-sly-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:


;;;### (autoloads nil "ac-sly" "ac-sly.el" (0 0 0 0))
;;; Generated autoloads from ac-sly.el

(defface ac-sly-menu-face '((t (:inherit ac-candidate-face))) "\
Face for slime candidate menu." :group 'auto-complete)

(defface ac-sly-selection-face '((t (:inherit ac-selection-face))) "\
Face for the slime selected candidate." :group 'auto-complete)

(defvar ac-source-sly-fuzzy '((init . ac-sly-init) (candidates . ac-source-sly-fuzzy-candidates) (candidate-face . ac-sly-menu-face) (selection-face . ac-sly-selection-face) (prefix . sly-symbol-start-pos) (symbol . "l") (match lambda (prefix candidates) candidates) (document . ac-sly-documentation)) "\
Source for fuzzy slime completion.")

(defvar ac-source-sly-simple '((init . ac-sly-init) (candidates . ac-source-sly-simple-candidates) (candidate-face . ac-sly-menu-face) (selection-face . ac-sly-selection-face) (prefix . sly-symbol-start-pos) (symbol . "l") (document . ac-sly-documentation) (match . ac-source-sly-case-correcting-completions)) "\
Source for slime completion.")

(autoload 'set-up-sly-ac "ac-sly" "\
Add an optionally-fuzzy slime completion source to `ac-sources'.

\(fn &optional FUZZY)" t nil)

(register-definition-prefixes "ac-sly" '("ac-s"))

;;;***

(provide 'ac-sly-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; ac-sly-autoloads.el ends here
