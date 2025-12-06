# Emacs Custom Keybindings Reference

This document provides a complete reference of all custom keybindings in Leo's Emacs configuration.

## Table of Contents
- [Theme & UI](#theme--ui)
- [Navigation & Movement](#navigation--movement)
- [Editing](#editing)
- [Completion & Minibuffer](#completion--minibuffer)
- [File & Buffer Management](#file--buffer-management)
- [Org-Mode & Literate Programming](#org-mode--literate-programming)
- [Common Lisp / Sly](#common-lisp--sly)
- [Project Management](#project-management)
- [Shell & Terminal](#shell--terminal)
- [Search & Replace](#search--replace)
- [Window Management](#window-management)
- [Git (Magit)](#git-magit)

---

## Theme & UI

| Keybinding | Command | Description |
|------------|---------|-------------|
| `F5` | `modus-themes-toggle` | Toggle between light (Operandi) and dark (Vivendi) Modus themes |
| `Alt-F10` | `toggle-frame-fullscreen` | Toggle fullscreen mode |
| `s-t` | `(lambda () (interactive))` | Disabled - prevents macOS font menu popup |

**Source**: `customizations/ui.el`

---

## Navigation & Movement

### Avy - Jump to Visible Text

| Keybinding | Command | Description |
|------------|---------|-------------|
| `M-j` | `avy-goto-char-timer` | Jump to any character with timed input (most useful) |
| `C-:` | `avy-goto-char` | Jump to a character (single char input) |
| `C-'` | `avy-goto-char-2` | Jump to a character (two char input for precision) |
| `M-g w` | `avy-goto-word-1` | Jump to the beginning of any visible word |
| `C-c C-j` | `avy-resume` | Resume last avy command |

### Window Navigation
n
| Keybinding | Command | Description |
|------------|---------|-------------|
| `S-<left/right/up/down>` | `windmove-*` | Move between windows using Shift + arrow keys |

### Line Movement & Manipulation

| Keybinding | Command | Description |
|------------|---------|-------------|
| `Home` (remapped) | `crux-move-beginning-of-line` | Move to first non-whitespace char, or line beginning if already there |
| `Shift-Return` | `crux-smart-open-line` | Open a new line below current line and move to it |
| `C-S-Return` | `crux-smart-open-line-above` | Open a new line above current line and move to it |
| `C-k` | `crux-smart-kill-line` | Kill line, or kill to end of line if not at beginning |
| `C-<backspace>` | `crux-kill-line-backwards` | Kill from cursor to beginning of line |
| `C-a` (remapped) | `crux-kill-whole-line` | Kill entire line regardless of cursor position |

**Source**: `customizations/navigation.el`

---

## Editing

### Text Manipulation

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-;` | `toggle-comment-on-line` | Comment or uncomment the current line |
| `C-c d` | `crux-duplicate-current-line-or-region` | Duplicate current line or region |
| `C-=` | `er/expand-region` | Expand selection by semantic units (word → expression → block) |
| `M-/` (remapped) | `hippie-expand` | Intelligent expansion using multiple completion sources |

### Search (Regular Expression by Default)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-s` | `isearch-forward-regexp` | Search forward using regular expressions |
| `C-r` | `isearch-backward-regexp` | Search backward using regular expressions |
| `C-M-s` | `isearch-forward` | Search forward (literal text) |
| `C-M-r` | `isearch-backward` | Search backward (literal text) |

### Eval & Replace (Emacs Lisp)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-e` | `crux-eval-and-replace` | Evaluate Emacs Lisp expression and replace it with result |

### Undo Tree

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-x u` | `undo-tree-visualize` | Open visual undo tree navigator |
| `C-_` / `C-/` | `undo-tree-undo` | Undo (standard) |
| `M-_` / `C-?` | `undo-tree-redo` | Redo |

**In undo-tree-visualizer:**
| Key | Description |
|-----|-------------|
| `p` / `n` | Move up/down in tree |
| `b` / `f` | Move left/right between branches |
| `t` | Toggle timestamps |
| `d` | Toggle diff display |
| `q` | Quit visualizer |

### macOS Specific

| Keybinding | Command | Description |
|------------|---------|-------------|
| `fn-Delete` | `delete-char` | Delete character forward (macOS) |
| `<pinch>` | `ignore` | Disabled - prevents accidental font size changes |
| `C-wheel-up/down` | `ignore` | Disabled - prevents accidental font size changes |

**Source**: `customizations/editing.el`

---

## Completion & Minibuffer

### Vertico (Minibuffer Completion)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-x M-r` | `vertico-repeat` | Repeat last minibuffer command |
| `C-l` (in minibuffer) | `vertico-directory-delete-word` | Delete directory component in file paths |
| `M-g` (in minibuffer) | `vertico-multiform-grid` | Switch to grid display |
| `M-q` (in minibuffer) | `vertico-multiform-flat` | Switch to flat display |

### Consult (Enhanced Commands)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `M-s f` | `consult-line` | Search lines in current buffer with live preview |
| `M-g g` | `consult-line` | Alternate binding for consult-line |
| `M-g o` | `consult-outline` | Navigate to outline headings |
| `M-g i` | `consult-imenu` | Navigate to imenu items (functions, classes, etc.) |
| `M-g r` | `consult-ripgrep` | Search files in project using ripgrep |
| `C-x C-r` | `consult-recent-file` | Open recently visited files |
| `C-x b` (remapped) | `consult-buffer` | Enhanced buffer switching |
| `M-y` (remapped) | `consult-yank-pop` | Browse kill ring with preview |
| `M-g M-g` (remapped) | `consult-goto-line` | Go to line number |
| `M-<up/down>` (in minibuffer) | `consult-history` | Browse minibuffer history |
| `TAB` (in isearch) | `vifon/isearch-to-consult-line` | Switch from isearch to consult-line |

### Embark (Contextual Actions)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c o` | `embark-act` | Show contextual actions for item at point |
| `C-.` | `embark-act` | Alternate binding for embark-act |
| `M-o` (in minibuffer) | `embark-act` | Show actions for current completion candidate |

### Marginalia

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-o` (in minibuffer) | `marginalia-cycle` | Cycle through annotation styles |

### Corfu (In-Buffer Completion)

Corfu provides auto-completion popups in buffers. It auto-activates after typing 2+ characters.

| Keybinding | Command | Description |
|------------|---------|-------------|
| `TAB` | `corfu-next` | Select next candidate in popup |
| `S-TAB` | `corfu-previous` | Select previous candidate in popup |
| `RET` | `corfu-insert` | Insert selected candidate |
| `M-n` / `M-p` | `corfu-next/previous` | Alternative navigation in popup |
| `C-g` | `corfu-quit` | Dismiss completion popup |
| `M-SPC` | `corfu-insert-separator` | Insert orderless separator for filtering |

**Note**: Corfu auto-completes with a 0.1s delay after typing 2+ characters. Cape provides additional completion backends (file paths, dabbrev).

**Source**: `customizations/completion.el`

---

## File & Buffer Management

### File Operations

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c f` | `crux-recentf-find-file` | Open recent file |
| `s-r` | `crux-recentf-ido-find-file` | Open recent file (ido-style, macOS) |
| `C-c r` | `crux-rename-file-and-buffer` | Rename current file and buffer |
| `C-c I` | `crux-find-user-init-file` | Open init.el |

### Buffer Management

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-x C-b` | `ibuffer` | List and manage buffers with ibuffer |
| `C-c k` | `crux-kill-other-buffers` | Kill all buffers except current one |

**Source**: `customizations/navigation.el`

---

## Org-Mode & Literate Programming

### Core Org-Mode Commands

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c l` | `org-store-link` | Store a link to the current location |
| `C-c A` | `org-agenda` | Open org agenda (uppercase A to avoid conflict with AOC) |
| `C-c c` | `org-capture` | Capture a note/task |

### Source Block Editing (Org-Babel)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-c` | `org-ctrl-c-ctrl-c` | Execute code block at point / toggle checkbox / evaluate table |
| `C-c '` | `org-edit-special` | Edit source block in dedicated buffer with full language mode |
| `C-c s` | `leo/insert-lisp-src-block` | Quick insert Common Lisp source block |

### Org-Babel Code Block Management

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-v t` | `org-babel-tangle` | Extract (tangle) code blocks to source files |
| `M-x org-babel-detangle` | `org-babel-detangle` | Sync changes from tangled file back to org file (reverse tangle) |
| `C-c C-v e` | `org-babel-execute-src-block` | Execute code block at point |
| `C-c C-v b` | `org-babel-execute-buffer` | Execute all code blocks in buffer |
| `C-c C-v C-s` | `org-babel-execute-subtree` | Execute all code blocks in current subtree |
| `C-c C-v d` | `org-babel-demarcate-block` | Split current code block at point |
| `C-c C-v v` | `org-babel-expand-src-block` | Expand code block (show all variables/noweb references) |
| `C-c C-v i` | `org-babel-view-src-block-info` | View information about current code block |
| `C-c C-v k` | `org-babel-remove-result` | Clear/remove results from code block |
| `C-c C-v n` | `org-babel-next-src-block` | Jump to next source block |
| `C-c C-v p` | `org-babel-previous-src-block` | Jump to previous source block |
| `C-c C-v g` | `org-babel-goto-named-src-block` | Jump to a named source block |
| `C-c C-v u` | `org-babel-goto-named-result` | Jump to a named result |
| `C-c C-v h` | `org-babel-describe-bindings` | Show all org-babel keybindings |
| `C-c C-v l` | `org-babel-load-in-session` | Load code block in session (evaluate without showing result) |
| `C-c C-v a` | `org-babel-sha1-hash` | Show hash of code block (useful for caching) |
| `C-c C-v x` | `org-babel-do-key-sequence-in-edit-buffer` | Execute key sequence as if in edit buffer |

### Navigation & Folding

| Keybinding | Command | Description |
|------------|---------|-------------|
| `TAB` | `org-cycle` | Cycle visibility of current heading (folded → children → subtree) |
| `S-TAB` | `org-global-cycle` | Cycle visibility of entire buffer |
| `C-c C-n` | `org-next-visible-heading` | Move to next heading |
| `C-c C-p` | `org-previous-visible-heading` | Move to previous heading |
| `C-c C-f` | `org-forward-heading-same-level` | Move to next heading at same level |
| `C-c C-b` | `org-backward-heading-same-level` | Move to previous heading at same level |
| `C-c C-u` | `org-up-element` | Move up to parent heading |

### Structure Editing

| Keybinding | Command | Description |
|------------|---------|-------------|
| `M-RET` | `org-meta-return` | Insert new heading/item at same level |
| `M-S-RET` | `org-insert-todo-heading` | Insert new TODO heading |
| `M-<left/right>` | `org-promote/demote-subtree` | Change heading level |
| `M-S-<left/right>` | `org-promote/demote-element` | Change element level (heading without subtree) |
| `M-<up/down>` | `org-move-subtree-up/down` | Move subtree up or down |

### Lists & Checkboxes

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-c` | (checkbox context) | Toggle checkbox state [ ] → [X] → [ ] |
| `C-c -` | `org-ctrl-c-minus` | Cycle list type (- → + → * → 1. → 1) ) |
| `S-<left/right>` | (checkbox context) | Cycle checkbox state |

### Template Expansion (org-tempo)

**Note:** org-tempo is enabled for easy block creation with `<X TAB` shortcuts.

| Template | Expansion | Description |
|----------|-----------|-------------|
| `<s TAB` | `#+begin_src ... #+end_src` | Generic source block (prompts for language) |
| `<sl TAB` | Common Lisp source block | Lisp source block with `:results output :exports both` (custom) |
| `<e TAB` | `#+begin_example ... #+end_example` | Example block (literal text, no syntax highlighting) |
| `<q TAB` | `#+begin_quote ... #+end_quote` | Quote block |
| `<v TAB` | `#+begin_verse ... #+end_verse` | Verse block (for poetry) |
| `<c TAB` | `#+begin_center ... #+end_center` | Centered text |
| `<C TAB` | `#+begin_comment ... #+end_comment` | Comment block |
| `<l TAB` | `#+begin_export latex ... #+end_export` | LaTeX export block |
| `<L TAB` | `#+latex: ` | Single-line LaTeX directive |
| `<h TAB` | `#+begin_export html ... #+end_export` | HTML export block |
| `<H TAB` | `#+html: ` | Single-line HTML directive |
| `<a TAB` | `#+begin_export ascii ... #+end_export` | ASCII export block |
| `<A TAB` | `#+ascii: ` | Single-line ASCII directive |
| `<i TAB` | `#+index: ` | Index entry |
| `<I TAB` | `#+include: ` | Include file directive |

### Export

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-e` | `org-export-dispatch` | Open export dispatcher menu |
| `C-c C-e h h` | Export to HTML | Export current org file to HTML |
| `C-c C-e h o` | Export to HTML and open | Export to HTML and open in browser |
| `C-c C-e l p` | Export to PDF | Export to PDF via LaTeX |

### Special Modes

**Source Edit Buffer** (when editing with `C-c '`):
| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c '` | `org-edit-src-exit` | Finish editing and return to org buffer |
| `C-c C-k` | `org-edit-src-abort` | Abort editing and discard changes |

**Notes:**
- org-tempo is enabled for easy template expansion (`<s TAB`, `<e TAB`, etc.)
- org-auto-tangle-mode automatically tangles files on save when `#+auto_tangle: t` is in the file header
- **Common Lisp specific configuration:**
  - Enabled languages: Common Lisp (lisp), Emacs Lisp, and Python
  - `org-babel-lisp-eval-fn #'sly-eval` ensures Sly is used (not SLIME)
  - Default header args for lisp blocks: `:results output :exports both :eval yes`
  - All lisp code blocks share the same Sly session for persistent state
- No separate ob-sly package required - ob-lisp.el supports both SLIME and Sly
- No confirmation is required before evaluating code blocks (set via `org-confirm-babel-evaluate nil`)
- Link comments are added to tangled files (`:comments link` default) for bidirectional sync
- Directories are created automatically when tangling (`:mkdirp yes` default)
- Use `org-babel-detangle` to sync changes from tangled files back to org source
- Mixed-pitch-mode provides variable-width fonts for prose while keeping code monospace
- Org-bullets provides prettier bullet characters (◉ ○ ●)

**Source**: `customizations/org-mode.el`

---

## Common Lisp / Sly

### Structural Editing: Lispy

**Note**: Lispy is enabled in lisp-mode. Single-key bindings activate when the cursor is at special positions (before/after parentheses) or when a region is active.

#### Lispy (Single-Key Bindings at Special Positions)

Lispy bindings activate when cursor is before `(` or after `)` or when region is active:

**Navigation:**
| Key | Command | Description |
|-----|---------|-------------|
| `j` | `lispy-down` | Move down into nested lists |
| `k` | `lispy-up` | Move up out of lists |
| `h` | `lispy-left` | Move to previous sibling s-expression |
| `l` | `lispy-right` | Move to next sibling s-expression |
| `f` | `lispy-flow` | Step inside a list (move forward into expression) |
| `[` / `]` | - | Jump to opening/closing parenthesis |

**List Manipulation:**
| Key | Command | Description |
|-----|---------|-------------|
| `>` | `lispy-slurp` | Slurp (pull next element into current list) |
| `<` | `lispy-barf` | Barf (push last element out of current list) |
| `r` | `lispy-raise` | Raise (replace parent with current s-expression) |
| `c` | `lispy-clone` | Clone (duplicate current s-expression) |
| `s` | `lispy-move-down` | Move s-expression down within parent |
| `w` | `lispy-move-up` | Move s-expression up within parent |
| `C` | `lispy-convolute` | Convolute (restructure nested expressions) |

**Other Useful Commands:**
| Key | Command | Description |
|-----|---------|-------------|
| `m` | `lispy-mark-list` | Mark the current list |
| `e` | `lispy-eval` | Evaluate the current expression |
| `a` | `lispy-ace-symbol` | Jump to symbol with ace-jump |
| `q` | `lispy-ace-paren` | Jump to parenthesis with ace-jump |
| `d` | `lispy-different` | Toggle between different bracket types |

**Special Position**: Place cursor right after `)` or right before `(` to activate single-key bindings.

### Quick Evaluation (REPL-Driven Development)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-e` | `sly-eval-last-expression` | Evaluate expression before cursor |
| `C-c C-r` | `sly-eval-region` | Evaluate selected region |
| `C-c C-f` | `sly-eval-defun` | Evaluate current function definition |
| `C-c C-b` | `sly-eval-buffer` | Evaluate entire buffer |
| `C-c C-p` | `sly-pprint-eval-last-expression` | Evaluate and pretty-print result |

### Debugging

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-s` | `sly-stickers-dwim` | Place/remove debug sticker (shows values inline) |

### Sly REPL (sly-mrepl-mode)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `TAB` | `completion-at-point` | Complete symbol at point (with Corfu popup) |
| `C-c C-z` | `sly-mrepl` | Switch to REPL buffer from source buffer |
| `C-c C-c` | `sly-interrupt` | Interrupt current evaluation |
| `C-c C-o` | `sly-mrepl-clear-recent-output` | Clear most recent output |
| `M-p` / `M-n` | `sly-mrepl-previous/next-input` | Navigate REPL history |
| `C-c M-o` | `sly-mrepl-clear-repl` | Clear entire REPL buffer |
| `,` | `sly-mrepl-shortcut` | Enter REPL shortcut (type `,help` for list) |

### Sly Extensions

Sly automatically loads with these extensions enabled:
- **sly-quicklisp**: `M-x sly-quicklisp-quickload` to install systems
- **sly-asdf**: `M-x sly-asdf-load-system` to load ASDF systems
- **sly-macrostep**: `M-x macrostep-expand` for interactive macro expansion
- **sly-stickers**: Interactive debugging - place stickers to see values as code executes

### Navigation & Definition Lookup

| Keybinding | Command | Description |
|------------|---------|-------------|
| `M-.` | `sly-edit-definition` | Jump to definition of symbol at point |
| `M-,` | `sly-pop-find-definition-stack` | Return from definition lookup |
| `C-c C-z` | `sly-mrepl` | Switch between source buffer and REPL |
| `C-c C-k` | `sly-compile-and-load-file` | Compile and load current file |
| `C-c C-c` | `sly-compile-defun` | Compile defun at point |

### Documentation & Help

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-d d` | `sly-describe-symbol` | Describe symbol at point |
| `C-c C-d a` | `sly-apropos` | Search for symbols matching pattern |
| `C-c C-d p` | `sly-apropos-package` | List symbols in package |
| `C-c C-d C-h` | `sly-hyperspec-lookup` | Look up symbol in Common Lisp HyperSpec (opens in w3m) |
| `C-c C-d ~` | `sly-hyperspec-lookup-format` | Look up format directive in HyperSpec |

### Advent of Code Helpers

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c a n` | `aoc/new-day` | Create new AoC day in `~/cl/AOC/YEAR/DayNN/DayNN.lisp` (traditional .lisp file, global) |
| `C-c a o` | `aoc/new-day-org` | Create new AoC day in `~/cl/AOC/YEAR/DayNN/DayNN.org` (literate programming with org-babel, global) |
| `C-c a 1` | `aoc/run-part1` | Run `day_NN-1` with input.txt and show result (in lisp buffer) |
| `C-c a 2` | `aoc/run-part2` | Run `day_NN-2` with input.txt and show result (in lisp buffer) |
| `C-c a e` | `aoc/run-example` | Run `day_NN-1` with `*example*` data (in lisp buffer) |

**Note:** Both `C-c a n` and `C-c a o` automatically download the input file from adventofcode.com if you have a session cookie configured in `~/.aoc-session`.

### Everybody Codes Helpers

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c e n` | `ec/new-quest` | Create new EC quest in `~/cl/EC/YEAR/questNN/qNN.lisp` (global) |
| `C-c e 1` | `ec/run-part1` | Run part 1 with input1.txt (in lisp buffer) |
| `C-c e 2` | `ec/run-part2` | Run part 2 with input2.txt (in lisp buffer) |
| `C-c e 3` | `ec/run-part3` | Run part 3 with input3.txt (in lisp buffer) |

### Yasnippet (Code Templates)

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c y` | `yas-expand` | Expand snippet at point |

Type the snippet key and use `C-c y` to expand:

| Snippet Key | Description |
|------------|-------------|
| `aocday` | Full day template matching Leo's style (fiveam tests, iterate, local-nicknames) |
| `grid` | Grid parsing functions using iterate (hash table with (row . col) keys) |

**Libraries automatically loaded**: fiveam, iterate, cl-ppcre, trivia, serapeum, str
**Local nicknames**: `:re` (cl-ppcre), `:sr` (serapeum), `:tr` (trivia), `:5a` (fiveam)

**Note**: Sly automatically connects when opening a Common Lisp file (.lisp, .cl, .asd)

**Source**: `customizations/lisp.el`, `customizations/aoc-helpers.el`, `customizations/ec-helpers.el`

---

## Project Management

### Projectile

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c p` | `projectile-command-map` | Open projectile command prefix |

Common projectile commands (after `C-c p`):

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c p f` | `projectile-find-file` | Find file in project |
| `C-c p p` | `projectile-switch-project` | Switch to another project |
| `C-c p d` | `projectile-find-dir` | Find directory in project |
| `C-c p g` | `projectile-grep` | Grep in project |
| `C-c p r` | `projectile-replace` | Replace in project |
| `C-c p k` | `projectile-kill-buffers` | Kill all project buffers |

### Treemacs (File Tree Sidebar)

Launch treemacs with `M-x treemacs`. Default keybindings in treemacs buffer:

| Keybinding | Command | Description |
|------------|---------|-------------|
| `M-0` | `treemacs-select-window` | Jump to treemacs window from anywhere |
| `?` | `treemacs-common-helpful-hydra` | Show help with available commands |
| `TAB` | `treemacs-TAB-action` | Expand/collapse directory at point |
| `RET` | `treemacs-RET-action` | Open file or expand directory |
| `q` | `treemacs-quit` | Hide treemacs window |
| `o a` | `treemacs-visit-node-ace` | Open file using ace-jump |
| `o h` | `treemacs-visit-node-in-most-recently-used-window` | Open in last used window |
| `r` | `treemacs-rename-file` | Rename file at point |
| `c f` | `treemacs-create-file` | Create new file |
| `c d` | `treemacs-create-dir` | Create new directory |
| `d` | `treemacs-delete-file` | Delete file/directory at point |
| `P` | `treemacs-peek-mode` | Toggle peek mode (preview files) |
| `w` | `treemacs-set-width` | Set treemacs window width |

**Note**: treemacs-projectile is also installed for project integration.

**Source**: `customizations/lisp.el`

---

## Shell & Terminal

### Launching Terminal

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c t` | `visit-term-buffer` | Open or switch to vterm terminal buffer |

### vterm Buffer Keybindings

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-t` | `vterm-copy-mode` | Toggle copy mode (for selecting/copying text) |
| `C-\` | `vterm-send-next-key` | Send next key literally to terminal |
| `C-c C-c` | `vterm-send-C-c` | Send Ctrl-C to terminal |
| `C-c C-z` | `vterm-undo` | Undo in vterm |
| `C-y` | `vterm-yank` | Paste from kill ring |
| `M-y` | `vterm-yank-pop` | Cycle through kill ring |

**In vterm copy mode:**
| Key | Description |
|-----|-------------|
| `SPC` | Start/end selection |
| `y` | Copy selection and exit copy mode |
| `q` | Exit copy mode |

**Note**: vterm is configured to use Fish shell

**Source**: `customizations/shell-integration.el`

---

## Search & Replace

### Built-in Search (remapped to use regexp by default)

See [Editing → Search](#search-regular-expression-by-default) section above.

### Consult-based Search

See [Completion & Minibuffer → Consult](#consult-enhanced-commands) section above.

---

## Window Management

### Winner Mode

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c <left>` | `winner-undo` | Undo window configuration changes |
| `C-c <right>` | `winner-redo` | Redo window configuration changes |

**Source**: Enabled globally in `customizations/ui.el`

---

## Git (Magit)

### Main Interface

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-x g` | `magit-status` | Open Magit status buffer (main interface) |

### Common Magit Commands (in magit-status buffer)

Once in the Magit status buffer, use these single-key commands:

**Staging/Unstaging:**
| Key | Command | Description |
|-----|---------|-------------|
| `s` | Stage | Stage file or hunk at point |
| `u` | Unstage | Unstage file or hunk at point |
| `S` | Stage all | Stage all unstaged changes |
| `U` | Unstage all | Unstage all staged changes |

**Committing:**
| Key | Command | Description |
|-----|---------|-------------|
| `c c` | Commit | Create a new commit |
| `c a` | Amend | Amend the last commit |
| `c e` | Extend | Extend the last commit (no message edit) |
| `c w` | Reword | Reword the last commit message |

**Branching:**
| Key | Command | Description |
|-----|---------|-------------|
| `b b` | Checkout | Checkout a branch |
| `b c` | Create | Create and checkout a new branch |
| `b n` | Create from | Create new branch from selected branch |

**Pushing/Pulling:**
| Key | Command | Description |
|-----|---------|-------------|
| `P p` | Push | Push current branch to remote |
| `P u` | Push -u | Push and set upstream |
| `F p` | Pull | Pull from remote |
| `f f` | Fetch | Fetch from remote |

**Viewing:**
| Key | Command | Description |
|-----|---------|-------------|
| `l l` | Log | View log for current branch |
| `l o` | Log other | View log for another branch |
| `d d` | Diff | Show diff for unstaged changes |
| `d s` | Diff staged | Show diff for staged changes |
| `TAB` | Toggle | Toggle visibility of file/hunk at point |

**Other Operations:**
| Key | Command | Description |
|-----|---------|-------------|
| `g` | Refresh | Refresh the status buffer |
| `q` | Quit | Close the Magit buffer |
| `?` | Help | Show command help |
| `$` | Process | Show git process buffer |

**Source**: `customizations/misc.el`

---

## Additional Notes

### Modes with Special Keybindings

- **which-key**: Displays available keybindings after you start typing a key sequence
- **show-paren-mode**: Highlights matching parentheses
- **undo-tree**: Visual undo/redo (see Undo Tree section in Editing)
- **aggressive-indent-mode**: Automatically indents code (excluded in text/markdown/yaml/html/python)
- **compile-angel**: Automatically byte-compiles and native-compiles Elisp files for better performance

### Disabled Keybindings

Some potentially dangerous or confusing keybindings are intentionally disabled:
- Font size changes via pinch/wheel gestures
- macOS system font menu (`s-t`)

### Configuration Files Reference

Each section above references its source file:
- `customizations/ui.el` - Theme and appearance
- `customizations/navigation.el` - Movement and navigation
- `customizations/editing.el` - Text editing
- `customizations/completion.el` - Completion frameworks (Vertico, Consult, Embark)
- `customizations/lisp.el` - Common Lisp and Sly configuration
- `customizations/shell-integration.el` - Terminal integration

---

**Last Updated**: 2025-12-05
**Configuration Location**: `~/.emacs.d/`

**Recent Changes**:
- Added Sly REPL keybindings (TAB completion, history navigation, shortcuts)
- Added Sly navigation/definition lookup keybindings (M-., M-,, C-c C-z, etc.)
- Added Sly documentation/help keybindings (C-c C-d prefix)
- Added Corfu in-buffer completion keybindings section
- Added Treemacs file tree sidebar keybindings
- Added Undo Tree keybindings with visualizer commands
- Added vterm keybindings (copy mode, sending keys, etc.)
- Expanded org-babel keybindings with navigation and result management
- Fixed enabled babel languages note (added Python)
- Added Magit section with comprehensive keybindings reference
