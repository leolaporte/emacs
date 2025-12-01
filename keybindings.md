# Emacs Custom Keybindings Reference

This document provides a complete reference of all custom keybindings in Leo's Emacs configuration.

## Table of Contents
- [Theme & UI](#theme--ui)
- [Navigation & Movement](#navigation--movement)
- [Editing](#editing)
- [Completion & Minibuffer](#completion--minibuffer)
- [File & Buffer Management](#file--buffer-management)
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

### Sly Extensions

Sly automatically loads with these extensions enabled:
- **sly-quicklisp**: `M-x sly-quicklisp-quickload` to install systems
- **sly-asdf**: `M-x sly-asdf-load-system` to load ASDF systems
- **sly-macrostep**: `M-x macrostep-expand` for interactive macro expansion
- **sly-stickers**: Interactive debugging - place stickers to see values as code executes

### HyperSpec Lookup

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c C-d C-h` | `sly-hyperspec-lookup` | Look up symbol in Common Lisp HyperSpec (opens in w3m) |

### Advent of Code Helpers

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c a n` | `aoc/new-day` | Create new AoC day in `~/cl/AOC/YEAR/DayNN/DayNN.lisp` (global) |
| `C-c a 1` | `aoc/run-part1` | Run `day_NN-1` with input.txt and show result (in lisp buffer) |
| `C-c a 2` | `aoc/run-part2` | Run `day_NN-2` with input.txt and show result (in lisp buffer) |
| `C-c a e` | `aoc/run-example` | Run `day_NN-1` with `*example*` data (in lisp buffer) |

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

**Source**: `customizations/lisp.el`

---

## Shell & Terminal

| Keybinding | Command | Description |
|------------|---------|-------------|
| `C-c t` | `visit-term-buffer` | Open or switch to vterm terminal buffer |

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
- **undo-tree**: `C-x u` opens visual undo tree
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

**Last Updated**: 2025-11-30
**Configuration Location**: `~/.emacs.d/`

**Recent Changes**:
- Added Magit section with comprehensive keybindings reference
- Removed Paredit documentation (not currently configured, only Lispy is active)
- Added Winner-mode section for window configuration undo/redo
- Added compile-angel to Additional Notes
- Updated source references to include ec-helpers.el
