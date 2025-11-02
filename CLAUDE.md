# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Overview

This is a personal Emacs configuration for Leo Laporte, based on the "emacs-for-clojure" setup with additions from Emacs community sources. The configuration uses **straight.el** as the package manager (not package.el) and organizes customizations into modular files under `customizations/`.

## Package Management

- **Package Manager**: straight.el (NOT package.el, which is disabled)
- **Installing Packages**: Use `straight-use-package` or `use-package` (which is configured to use straight.el by default via `straight-use-package-by-default t`)
- **Package Location**: Packages are stored in `straight/repos/` and built in `straight/build/`
- When adding new packages, always use the `use-package` macro with `:straight t` (or rely on the default)

## Configuration Architecture

The main `init.el` bootstraps straight.el and loads modular configuration files from `customizations/`:

1. **completion.el** - Modern completion framework (Vertico, Consult, Embark, Orderless, Marginalia, Corfu) replacing the commented-out Helm setup
2. **misc.el** - Git integration (Magit) and miscellaneous settings
3. **shell-integration.el** - vterm terminal emulator setup with Fish shell integration
4. **lisp.el** - Common Lisp development (Sly, lispy, auto-complete, projectile)
5. **editing.el** - Editor behavior (aggressive-indent, undo-tree, markdown-mode, save/backup settings)
6. **navigation.el** - Navigation tools (which-key, avy, crux, expand-region)
7. **ui.el** - Visual appearance (Modus themes, fonts, window sizing)
8. **custom.el** - Emacs-generated customizations (via `custom-file`)

**Commented out**: org-mode.el, init-helm.el, setup-clojure.el (may be enabled selectively)

## Key Technical Decisions

### Completion Stack
The configuration uses a modern "completing-read" framework:
- **Vertico**: Minibuffer completion UI with extensions (multiform, posframe)
- **Consult**: Enhanced completion commands (consult-line, consult-buffer, consult-ripgrep)
- **Embark**: Contextual actions on completion candidates
- **Orderless**: Flexible matching with special dispatchers (! for exclusion, = for literal)
- **Marginalia**: Rich annotations in minibuffer
- **Corfu**: Completion-at-point popup

### Lisp Development
- **REPL**: Sly (not SLIME) for Common Lisp with SBCL
  - macOS: `/opt/homebrew/bin/sbcl`
  - Linux: `/usr/bin/sbcl`
- **Navigation**: lispy mode for structural editing
- **Auto-completion**: ac-sly integrated with auto-complete
- **Window Management**: Vertical splits preferred for REPL (configured via `split-width-threshold`)
- **Projectile**: Project navigation enabled globally

### Shell Integration
- **Terminal**: vterm (requires Emacs built with module support)
- **Shell**: Fish shell
  - macOS: `/opt/homebrew/bin/fish`
  - Linux: `/usr/bin/fish`
- **Keybinding**: `C-c t` opens or switches to vterm buffer

### Theme & UI
- **Theme**: Modus Vivendi (dark) with extensive customization for accessibility (deuteranopia support)
- **Font**: Iosevka Nerd Font Mono at height 220
- **Toggle**: F5 switches between light (Operandi) and dark (Vivendi)
- **Minimal UI**: No menu bar, tool bar, or scroll bars

## Modifying Configuration

When editing configuration files:

1. **Adding packages**: Use `use-package` declarations in the appropriate customization file
2. **Platform-specific code**: Check for `(eq system-type 'darwin)` for macOS vs Linux differences
3. **Performance**: Note the GC threshold is set to 50MB and straight.el checks modifications only on save
4. **Reload**: After changes, either restart Emacs or manually `load` the modified file

## Important Paths

- Custom file: `~/.emacs.d/customizations/custom.el`
- Auto-save: `~/.emacs.d/.auto-save-list/`
- Backups: `~/.emacs.d/backups/`
- Recent files: `~/.emacs.d/.recentf`
- Saved places: `~/.emacs.d/places`

## Notable Keybindings

- `C-c t`: Open/switch to vterm
- `C-c p`: Projectile command map
- `F5`: Toggle Modus theme (light/dark)
- `M-j`: avy-goto-char-timer (jump to character)
- `C-c o` or `C-.`: Embark act
- `C-;`: Toggle comment on line
- `C-=`: Expand region

## Development Workflow

The configuration is optimized for:
- Common Lisp development with Sly REPL
- Git workflows via Magit
- Terminal access via vterm
- Modern completion and navigation throughout
