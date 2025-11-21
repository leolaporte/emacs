# Leo Laporte's Emacs Configuration

**2017-2025** — A constantly evolving work-in-progress, ~~stolen~~
borrowed from the best.

## Overview

My personal Emacs yak shave optimized for Common Lisp development,
with a focus on Advent of Code and Everybody.Codes. Features automatic
compilation, persistent window geometry, and a complete modern
completion stack. Refactored with considerable help from Claude
Code. Requires Emacs version >29 for some features.

## Key Features

- **Package Management**: use-package with MELPA (migrated from straight.el Nov 2025)
- **Performance**: Automatic byte and native compilation via compile-angel.el (2.5-5x speedup)
- **Completion**: Modern stack with Vertico, Consult, Embark, Marginalia, Orderless, Corfu (thank you (minad)[https://github.com/sponsors/minad])
- **Common Lisp**: Sly with quicklisp/asdf/macrostep extensions, auto-connect REPL
- **Structural Editing**: Choose from Paredit (traditional) and Lispy (vi-style single-key navigation) (I prefer Lispy these days.)
- **Navigation**: Avy jump-to-char, Crux utilities, which-key discovery
- **Git Integration**: Magit
- **Terminal**: vterm with Fish shell (e-shell for most simple shell
  commands)
- **Theme**: Modus Vivendi (dark)/Operandi (light) (toggle with F5) (requires Emacs 28+)
- **Window Persistence**: Automatically remembers and restores window position/size, Sly REPL positioning

## Quick Start

```bash
git clone https://github.com/leolaporte/emacs.git ~/.emacs.d
emacs
```

On first launch, all packages will be automatically built from
MELPA. On Emacs 31 there are some warnings during compilation but they
can be safely ignored. All .el files must support lexical
binding. (M-x elisp-enable-lexical-binding)

## Documentation

- **CLAUDE.md** — Architecture, workflows, and guidance for AI assistants
- **keybindings.md** — Comprehensive keybinding reference (360+
  lines) - I keep this up to date with Claude's help
- See `customizations/` directory for modular configuration files

## Advent of Code / Everybody Codes

Specialized helpers for competitive programming:
- `C-c a n` — Create new AoC day with template
- `C-c a 1/2/e` — Run part 1/2/example
- `C-c e n` — Create new EC quest
- Automatic input file downloading for AoC
- Slowly adding YASnippets for boilerplate code
- 45+ yasnippet templates for Common Lisp patterns

## Recent Updates

### 2025-11-17 Major Refactoring
- Migrated from straight.el back to good ol' use-package + package.el
- Added compile-angel.el for automatic compilation
- Enabled native compilation (2.5-5x performance boost)
- Added frame geometry persistence (saves and restores window position
  in a "frame-geometry" file in the .emacs directory )
- Enabled lispy-mode alongside paredit (you can use both but Lispy is my preference)
- Created comprehensive CLAUDE.md documentation
- Updated all packages to use-package format
- Freed 271MB by removing straight/ directory

### 2025-11-02 Massive Cleanup
- Added paredit for structural editing
- Removed unused customizations
- Added automatic templates for Advent of Code and Everybody Codes via Claude Code

## Structure

```
~/.emacs.d/
├── early-init.el          # Pre-GUI initialization, native-comp settings
├── init.el                # Main entry point, bootstraps use-package
├── customizations/        # Modular configuration
│   ├── ui.el             # Theme, fonts, frame geometry
│   ├── editing.el        # Text editing, aggressive-indent, undo-tree
│   ├── navigation.el     # Avy, crux, expand-region, which-key
│   ├── completion.el     # Vertico/Consult/Embark stack
│   ├── lisp.el           # Sly, paredit, lispy, yasnippet
│   ├── shell-integration.el  # vterm, Fish shell
│   ├── misc.el           # Magit, compile-angel
│   ├── aoc-helpers.el    # Advent of Code automation
│   └── ec-helpers.el     # Everybody Codes automation
└── snippets/lisp-mode/   # 45+ Common Lisp yasnippet templates
```

## Package Management

Update packages: `M-x package-list-packages` then `U x`
Refresh package list: `M-x package-refresh-contents`

## License

Personal configuration — feel free to steal anything useful!

## Credits

Built with inspiration from:
- [flyingmachine/emacs-for-clojure](https://github.com/flyingmachine/emacs-for-clojure)
- [Emacs Redux](https://emacsredux.com)
- [Mastering Emacs](https://www.masteringemacs.org) by Mickey Petersen
- [r/emacs](https://reddit.com/r/emacs) community

With assistance from Claude Code.
