;; -*- lexical-binding: t; -*-

;;;; Advent of Code Helper Functions and Configuration
;;;; Leo Laporte - November 2025 (with help from Claude Code)


;; Configuration for AOC session cookie
(defvar aoc/session-cookie-file "~/.aoc-session"
  "File containing the Advent of Code session cookie.
Get this from your browser after logging into adventofcode.com.
The cookie value is the 'session' cookie value (a long hex string).")

;; Function to download input file from adventofcode.com
(defun aoc/download-input (year day)
  "Download input file for YEAR and DAY from adventofcode.com.
Requires a valid session cookie in ~/.aoc-session.
To get your session cookie:
1. Log into adventofcode.com in your browser
2. Open developer tools (F12)
3. Go to Application/Storage > Cookies > https://adventofcode.com
4. Copy the value of the 'session' cookie
5. Save it to ~/.aoc-session"
  (let* ((session-cookie-file (expand-file-name aoc/session-cookie-file))
         (session-cookie (when (file-exists-p session-cookie-file)
                          (with-temp-buffer
                            (insert-file-contents session-cookie-file)
                            (string-trim (buffer-string)))))
         (url (format "https://adventofcode.com/%d/day/%d/input" year day))
         (output-file (expand-file-name (format "~/cl/AOC/%d/Day%02d/input.txt" year day))))
    (if (and session-cookie (not (string-empty-p session-cookie)))
        (let ((dir (file-name-directory output-file)))
          (unless (file-exists-p dir)
            (make-directory dir t))
          (message "Downloading from: %s" url)
          (message "Saving to: %s" output-file)
          ;; Use a temp buffer to capture curl output
          (let* ((temp-buffer (generate-new-buffer " *aoc-download*"))
                 (command (format "curl -f -s -S --no-progress-meter -b 'session=%s' '%s'"
                                session-cookie url))
                 (exit-code (call-process-shell-command command nil temp-buffer)))
            (if (= exit-code 0)
                (progn
                  ;; Write the buffer contents to file
                  (with-current-buffer temp-buffer
                    (write-region (point-min) (point-max) output-file))
                  (kill-buffer temp-buffer)
                  (let ((size (nth 7 (file-attributes output-file))))
                    (if (> size 0)
                        (progn
                          (message "✓ Downloaded input file for %d day %d (%d bytes)" year day size)
                          t)
                      (progn
                        (message "✗ Downloaded file is empty. Check your session cookie.")
                        (message "File content: %s"
                                (with-temp-buffer
                                  (insert-file-contents output-file)
                                  (buffer-string)))
                        (delete-file output-file)
                        nil))))
              (progn
                ;; Show the error output
                (let ((error-output (with-current-buffer temp-buffer
                                     (buffer-string))))
                  (message "✗ curl failed (exit code %d): %s" exit-code error-output)
                  (kill-buffer temp-buffer)
                  nil)))))
      (progn
        (message "✗ No session cookie found at: %s" session-cookie-file)
        (message "See https://github.com/wimglenn/advent-of-code-wim/issues/1 for instructions")
        nil))))

;; Quick function to create new AoC day file
(defun aoc/new-day (year day)
  "Create a new Advent of Code solution file for YEAR and DAY.
Uses yasnippet template from ~/.emacs.d/snippets/lisp-mode/aoc-day-new.
Automatically downloads the input file from adventofcode.com if session cookie is configured."
  (interactive "nYear: \nnDay: ")
  (let* ((dir (format "~/cl/AOC/%d/Day%02d" year day))
         (file (format "%s/Day%02d.lisp" dir day))
         (input-file (format "%s/input.txt" dir))
         (template-file (expand-file-name "~/.emacs.d/snippets/lisp-mode/aoc-day-new"))
         (day-str (format "%02d" day))
         (current-date (format-time-string "%d %b %Y at %H:%M")))
    (unless (file-exists-p dir)
      (make-directory dir t))

    ;; Download input file if it doesn't exist
    (unless (file-exists-p input-file)
      (message "Downloading input file for %d day %d..." year day)
      (aoc/download-input year day))

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
            ;;    Pattern matches ${1:$ followed by anything until the closing }
            (setq content (replace-regexp-in-string
                          "\\${1:\\$[^}]*}"
                          (format "%d" year)
                          content))

            ;; 2. Replace ${2:$(expression)} with zero-padded day
            (setq content (replace-regexp-in-string
                          "\\${2:\\$[^}]*}"
                          day-str
                          content))

            ;; 3. Replace ${1:default} with year - simple default values
            (setq content (replace-regexp-in-string
                          "\\${1:[0-9]+}"
                          (format "%d" year)
                          content))

            ;; 4. Replace ${2:default} with day - simple default values
            (setq content (replace-regexp-in-string
                          "\\${2:[0-9]+}"
                          (format "%d" day)
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

;; Quick function to run current AoC solution
(defun aoc/run-part (part)
  "Run PART (1 or 2) on the current buffer's day."
  (interactive "nPart (1 or 2): ")
  (let* ((pkg (sly-current-package))
         ;; Extract day number from package name like :day01
         (day (if (string-match "day\\([0-9]+\\)" pkg)
                  (match-string 1 pkg)
                "01"))
         (func (intern (format "DAY_%s-%d" day part) pkg)))
    (sly-eval-async `(,func (uiop:read-file-lines
                             (symbol-value (intern "*DATA-FILE*" ,pkg))))
      (lambda (result)
        (message "Part %d result: %S" part result)))))

(defun aoc/run-part1 ()
  "Run part 1 of current AoC solution."
  (interactive)
  (aoc/run-part 1))

(defun aoc/run-part2 ()
  "Run part 2 of current AoC solution."
  (interactive)
  (aoc/run-part 2))

(defun aoc/run-example ()
  "Run part 1 with example from *example* parameter."
  (interactive)
  (let* ((pkg (sly-current-package))
         (day (if (string-match "day\\([0-9]+\\)" pkg)
                  (match-string 1 pkg)
                "01"))
         (func (intern (format "DAY_%s-1" day) pkg)))
    (sly-eval-async `(,func (symbol-value (intern "*EXAMPLE*" ,pkg)))
      (lambda (result)
        (message "Example result: %S" result)))))

;; Keybindings for AoC workflow
;; C-c a n is global - can create new day from anywhere
(global-set-key (kbd "C-c a n") 'aoc/new-day)

;; These require a lisp buffer context
(with-eval-after-load 'sly
  (define-key sly-mode-map (kbd "C-c a 1") 'aoc/run-part1)
  (define-key sly-mode-map (kbd "C-c a 2") 'aoc/run-part2)
  (define-key sly-mode-map (kbd "C-c a e") 'aoc/run-example))
