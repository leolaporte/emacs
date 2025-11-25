# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is Leo Laporte's personal Emacs configuration (2017-2025), optimized for Common Lisp development with a focus on Advent of Code and Everybody Codes competitive programming. The configuration uses a modular architecture with dedicated files for different concerns and relies on elpaca for fast, reproducible package management.

## Architecture

### Module System

Configuration is split into focused modules loaded from `customizations/`:

- **ui.el** - Theme (Modus Vivendi/Operandi), fonts (Iosevka Nerd Font), visual settings
- **editing.el** - Text manipulation, aggressive-indent, undo-tree, markdown support
- **navigation.el** - Avy (jump-to-char), Crux utilities, windmove, which-key
- **completion.el** - Modern completion stack (Vertico/Consult/Embark/Marginalia/Orderless/Corfu)
- **lisp.el** - Common Lisp development with Sly, Paredit, Rainbow delimiters, auto-complete
- **shell-integration.el** - vterm with Fish shell
- **aoc-helpers.el** - Advent of Code workflow automation
- **ec-helpers.el** - Everybody Codes workflow automation
- **misc.el** - Magit and other utilities
- **custom.el** - Auto-generated customizations

### Load Order

1. `early-init.el` - Sets GC threshold for faster startup, disables package.el
2. `init.el` - Bootstraps elpaca and use-package integration, loads customization modules in sequence
3. All module files have lexical binding enabled

## Package Management

Uses **elpaca** package manager with **use-package** integration:
- Update packages: `M-x elpaca-update-all` or update specific package with `M-x elpaca-update`
- Rebuild packages: `M-x elpaca-rebuild` (useful after elpaca updates)
- Browse packages: `M-x elpaca-browse` (interactive package browser)
- View package status: `M-x elpaca-manager` (shows installed packages and their status)
- All packages are declared with `use-package` and installed automatically via `:ensure t` (set globally)
- Packages are cloned from their source repositories (GitHub, GitLab, etc.) and built locally
- Benefits: Faster startup, reproducible builds, easy to pin package versions, parallel installation

### Performance Optimization

**compile-angel.el** automatically byte-compiles and native-compiles all Elisp files for better performance:
- Byte-compilation: Creates `.elc` files for faster loading
- Native-compilation: Creates `.eln` files (2.5-5x faster than byte-compiled)
- Automatic: Files are compiled on load and when saved
- Configuration: `compile-angel-on-load-mode` enabled globally
- Excluded files: `init.el` and `early-init.el` are not auto-compiled to avoid recursion

## Common Lisp Development Workflow

### Sly Configuration
- Automatically connects when opening `.lisp`, `.cl`, or `.asd` files
- Extensions enabled: sly-quicklisp, sly-asdf, sly-macrostep, sly-stickers
- HyperSpec lookup via w3m browser (in-Emacs)
- Paredit for structural editing (slurp/barf with `C-)`, `C-}`, etc.)

### Standard Libraries
Leo's code typically uses these libraries with local nicknames:
```lisp
(:local-nicknames (:re :cl-ppcre)
                  (:sr :serapeum)
                  (:tr :trivia)
                  (:5a :fiveam))
```
Common packages: fiveam, iterate, cl-ppcre, trivia, serapeum, str

### Coding Style
- Test framework: FiveAM with `*run-test-when-defined* t`
- Iteration: `iterate` package (not `loop`)
- Grid data structures: Hash tables with `(row . col)` cons keys
- File structure: Functions first, then tests at bottom

## Advent of Code / Everybody Codes Workflows

### AOC Commands (C-c a prefix)
- `C-c a n` - Create new day in `~/cl/AOC/YEAR/Day_NN/Day_NN.lisp`
- `C-c a 1` - Run part 1 with `input.txt`
- `C-c a 2` - Run part 2 with `input.txt`
- `C-c a e` - Run part 1 with example data

### EC Commands (C-c e prefix)
- `C-c e n` - Create new quest in `~/cl/EC/YEAR/Quest_NN/Quest_NN.lisp`
- `C-c e 1/2/3` - Run parts 1, 2, or 3

### Input File Management
- AOC: Automatically downloads from adventofcode.com using session cookie in `~/.aoc-session`
- Template system uses yasnippet (`aocday` and `ecquest` snippets)
- Templates include FiveAM test setup, common libraries, and part 1/2 function stubs

## Key Navigation Patterns

- **Avy jump**: `M-j` (timed char jump - most useful)
- **Buffer switching**: `C-x b` (Consult)
- **Search in buffer**: `M-s f` or `M-g g` (consult-line with live preview)
- **Project search**: `M-g r` (consult-ripgrep)
- **Recent files**: `C-x C-r` (Consult)
- **Window navigation**: Shift + arrow keys
- **Undo window changes**: `C-c left/right` (winner-mode)

## Important Keybindings for Lisp Development

- **Evaluate**: `C-c C-e` (last expr), `C-c C-f` (defun), `C-c C-b` (buffer)
- **Paredit slurp/barf**: `C-)`, `C-}`, `C-(`, `C-{`
- **Expand region**: `C-=` (expand by semantic units)
- **Comment line**: `C-;`
- **Smart line operations**: `Home` (smart beginning), `Shift-Return` (open below), `C-S-Return` (open above)

See `keybindings.md` for comprehensive reference (320 lines).

## Editing Emacs Configuration

### When Modifying Configuration Files
1. All `.el` files must have `;; -*- lexical-binding: t; -*-` at the top
2. Modules follow use-package pattern:
   ```elisp
   (use-package package-name
     :init    ; Code to run before loading
     :config  ; Code to run after loading
     :bind    ; Key bindings
     :hook    ; Mode hooks
     :custom  ; Custom variables
     :after)  ; Load after other packages
   ```
3. Use `load` in init.el (not `require`) for customization modules
4. Custom keybindings: Update both code and `keybindings.md`
5. Keep module separation: UI code in ui.el, Lisp code in lisp.el, etc.

### Testing Changes
- Restart Emacs or `M-x eval-buffer` in the modified file
- Check for errors in `*Messages*` buffer
- For package issues: `M-x elpaca-manager` to view status, then use `M-x elpaca-update` to update
- Rebuild a package: `M-x elpaca-rebuild` (select specific package)
- Reinstall a package: `M-x elpaca-delete` then restart Emacs (elpaca will reinstall via `:ensure t`)

## File Organization Standards

### Advent of Code
```
~/cl/AOC/YEAR/Day_NN/
├── Day_NN.lisp    # Main solution file
└── input.txt      # Puzzle input (auto-downloaded)
```

### Everybody Codes
```
~/cl/EC/YEAR/Quest_NN/
├── Quest_NN.lisp  # Main solution file
├── input1.txt     # Part 1 input
├── input2.txt     # Part 2 input
└── input3.txt     # Part 3 input
```

## Notable Configuration Details

- **Theme toggle**: `F5` switches between light/dark Modus themes
- **Search**: Regex search is default (`C-s`), literal search is `C-M-s`
- **Aggressive indent**: Enabled globally except in text/markdown/yaml/html/org modes
- **Completion**: Vertico for minibuffer, Corfu for in-buffer
- **Terminal**: vterm with Fish shell (`C-c t`)
- **macOS compatibility**: Clipboard integration, shell PATH loading via exec-path-from-shell

## Common Tasks

### Adding New Yasnippet Templates
1. Create file in `~/.emacs.d/snippets/lisp-mode/`
2. Format: 4-line header (key, name, contributor, condition), then template body
3. Use `$0` for cursor position, `${1:placeholder}` for tab stops

### Managing Unused Configurations
Move to `customizations/unused/` directory rather than deleting. Currently unused: Helm, Clojure, JavaScript, Julia, Org-mode, Keychain, Blog configurations.

## Git Workflow

- Main branch: `main`
- Recent focus: Lexical bindings cleanup, AOC/EC helper automation
- Configuration actively maintained and evolved

## Reference Files

- **keybindings.md** - Comprehensive keybinding reference with descriptions and sources
- **README.md** - Brief overview and history
- **snippets/** - 45+ yasnippet templates for Common Lisp coding patterns
