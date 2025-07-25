# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Repository Overview

This is an Emacs configuration repository using a literate programming approach with Org mode. The configuration is written in `config.org` and automatically tangled to Elisp files on startup.

## Architecture

The Emacs configuration follows this loading sequence:

1. `init.el` - Entry point that loads Org files in order
2. `package-bootstrap.org` - Package manager setup (MELPA, use-package)
3. `config.org` - Main configuration file containing all settings
4. Local org files (gitignored):
   - `local-bootstrap.org` - Machine-specific pre-config
   - `local-before.org` - Local settings loaded before main config
   - `local-after.org` - Local settings loaded after main config

## Key Commands

### Regenerating Configuration
```bash
# The config.org file is automatically tangled on startup
# To manually regenerate config.el from config.org:
M-x org-babel-tangle
```

### Project Commands (defined in config.org)

**Go Projects:**
- `C-c m b` - Compile Go project (`make build`)
- `C-c m t` - Test Go project (`make test`)
- `C-c m l` - Lint Go project (`make lint`)
- `C-c m f` - Format Go project (`make fmt`)

**TypeScript Projects:**
- `C-c m B` - Build TypeScript project (`turbo build`)
- `C-c m T` - Test TypeScript project (`turbo test`)
- `C-c m d` - Start dev server (`turbo dev`)

**Testing Individual Files:**
- Go: `C-c t f` - Test current file
- Go: `C-c t t` - Test at point
- TypeScript: `C-c t j` - Jest popup

## Configuration Structure

The `config.org` file is organized into these major sections:

1. **Mac Magic** - macOS-specific keybindings and PATH setup
2. **Configure Completions** - Vertico, Consult, Marginalia setup
3. **Appearance** - Themes, fonts, frame settings
4. **Navigation** - Window management, buffer switching
5. **Editing** - Text editing features, LSP configuration
6. **Utilities** - Helper functions and tools
7. **Modes** - Language-specific configurations

## Important Configuration Details

### EditorConfig Support
The repository includes `.editorconfig` support via the `editorconfig` package. Emacs will automatically respect `.editorconfig` files in projects.

### LSP Configuration
- Uses `lsp-mode` for most languages
- `eglot` is available as an alternative
- Language servers configured for: Go, TypeScript, Rust, C/C++, Python

### Package Management
- Uses `use-package` for declarative package configuration
- Auto-updates packages daily at 3:00 AM
- Packages installed from MELPA and GNU ELPA

### Key Bindings Framework
- Hyper key (Cmd on Mac) for window management
- `C-c` prefix for user-defined commands
- Project management via Projectile (`C-c p` prefix)

## Development Workflow

1. Make changes to `config.org` (not the generated `.el` files)
2. Either restart Emacs or run `M-x org-babel-load-file` on config.org
3. Test changes before committing
4. Generated files (`config.el`, `package-bootstrap.el`) are included in the repository