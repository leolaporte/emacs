;;;; Everybody Codes Helper Functions
;;;; Leo Laporte - November 2025 (with help from Claude Code)

;; Quick function to create new EC quest file
(defun ec/new-quest (year quest)
  "Create a new Everybody Codes solution file for YEAR and QUEST.
Uses yasnippet template from ~/.emacs.d/snippets/lisp-mode/ec-quest-new."
  (interactive "nYear: \nnQuest: ")
  (let* ((quest-str (format "%02d" quest))
         (dir (format "~/cl/ec/%d/quest%s" year quest-str))
         (file (format "%s/q%s.lisp" dir quest-str))
         (template-file (expand-file-name "~/.emacs.d/snippets/lisp-mode/ec-quest-new"))
         (current-date (format-time-string "%d %b %Y at %H:%M")))
    (unless (file-exists-p dir)
      (make-directory dir t))

    (find-file file)
    (when (= (buffer-size) 0)
      (if (file-exists-p template-file)
          (let ((content (with-temp-buffer
                          (insert-file-contents template-file)
                          ;; Skip the yasnippet header (first 4 lines)
                          (goto-char (point-min))
                          (forward-line 4)
                          (buffer-substring (point) (point-max)))))
            ;; Replace yasnippet fields in order: complex expressions first, then simple defaults

            ;; 1. Replace ${1:$(expression)} with year - match the elisp expression forms
            (setq content (replace-regexp-in-string
                          "\\${1:\\$[^}]*}"
                          (format "%d" year)
                          content))

            ;; 2. Replace ${2:$(expression)} with zero-padded quest
            (setq content (replace-regexp-in-string
                          "\\${2:\\$[^}]*}"
                          quest-str
                          content))

            ;; 3. Replace ${1:default} with year - simple default values
            (setq content (replace-regexp-in-string
                          "\\${1:[0-9]+}"
                          (format "%d" year)
                          content))

            ;; 4. Replace ${2:default} with quest - simple default values
            (setq content (replace-regexp-in-string
                          "\\${2:[0-9]+}"
                          (format "%d" quest)
                          content))

            ;; 5. Replace backtick expressions for date
            (setq content (replace-regexp-in-string
                          "`(format-time-string[^`]+)`"
                          current-date
                          content))

            ;; 6. Remove the $0 cursor placeholder
            (setq content (replace-regexp-in-string "\\$0" "" content))

            (insert content))
        (message "Template file not found: %s" template-file)))))

;; Quick function to run EC quest parts
(defun ec/run-part (part)
  "Run PART (1, 2, or 3) on the current buffer's quest."
  (interactive "nPart (1, 2, or 3): ")
  (let* ((pkg (sly-current-package))
         ;; Extract quest number from package name like :ec.2024.quest01
         (quest (if (string-match "quest\\([0-9]+\\)" pkg)
                    (match-string 1 pkg)
                  "01"))
         (func (intern (format "QUEST_%s-%d" quest part) pkg))
         (data-file (intern (format "*DATA-FILE%d*" part) pkg)))
    (sly-eval-async `(,func (uiop:read-file-lines
                            (symbol-value (intern ,(format "*DATA-FILE%d*" part) ,pkg))))
      (lambda (result)
        (message "Part %d result: %S" part result)))))

(defun ec/run-part1 ()
  "Run part 1 of current EC quest."
  (interactive)
  (ec/run-part 1))

(defun ec/run-part2 ()
  "Run part 2 of current EC quest."
  (interactive)
  (ec/run-part 2))

(defun ec/run-part3 ()
  "Run part 3 of current EC quest."
  (interactive)
  (ec/run-part 3))

;; Keybindings for EC workflow
(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c e 1") 'ec/run-part1)
  (define-key sly-mode-map (kbd "C-c e 2") 'ec/run-part2)
  (define-key sly-mode-map (kbd "C-c e 3") 'ec/run-part3)
  (define-key sly-mode-map (kbd "C-c e n") 'ec/new-quest))
