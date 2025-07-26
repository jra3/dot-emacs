# Orphaned Packages Analysis

## Packages to Remove

### 1. Removed from Config but Still Installed
- **helm** and related (helm-core, helm-descbinds, helm-flx, helm-xref) - Migrated to Vertico
- **company** - Replaced with Corfu
- **anzu** - Using built-in isearch-lazy-count
- **jade-mode** - Replaced with pug-mode
- **tree-sitter**, **tree-sitter-langs**, **tsc**, **ts** - Using built-in treesit
- **popwin** - Replaced with native display-buffer-alist
- **org-pomodoro** - Removed clocking functionality
- **ace-window** - Not referenced anywhere in config

### 2. Potentially Unused Packages
- **2048-game** - Game, likely not needed
- **alert** - Dependency of org-pomodoro?
- **button-lock** - No references found
- **cython-mode** - No Python/Cython config found
- **flx** - Was for Helm fuzzy matching
- **gntp** - Growl notifications, likely unused
- **google-c-style** - No references in config
- **jade-mode** - Replaced by pug-mode
- **magit-popup** - Old Magit dependency
- **modern-cpp-font-lock** - Was in removed C++ section
- **rainbow-mode** - Different from rainbow-delimiters
- **vterm** - Terminal emulator, check if used
- **wfnames** - No references found

### 3. Orphaned Custom Variables
In custom.el:
- `org-pomodoro-format` - No longer needed
- `org-pomodoro-play-sounds` - No longer needed
- `org-clock-task-overrun-text` - No longer needed

## Packages to Keep

### Confirmed in Use
All packages referenced in config.org with `use-package`

### Dependencies to Check
- **async** - Likely needed by other packages
- **dash**, **f**, **s**, **ht** - Common dependencies
- **lv** - Likely for lsp-ui
- **spinner** - Progress indicators

## Cleanup Commands

```bash
# Remove specific packages
cd ~/.emacs.d/elpa
rm -rf helm* company* anzu* jade-mode* tree-sitter* popwin* org-pomodoro* ace-window*

# Remove potentially unused
rm -rf 2048-game* alert* button-lock* cython-mode* flx* gntp* google-c-style* magit-popup* modern-cpp-font-lock* wfnames*
```

## Custom.el Cleanup

Remove these lines from custom.el:
```elisp
'(org-clock-task-overrun-text "Don't be such a punk")
'(org-pomodoro-format "P~%s")
'(org-pomodoro-play-sounds nil)
```

## Verification Steps

1. After removal, restart Emacs and check for errors
2. Run `M-x package-autoremove` to clean up dependencies
3. Check if any removed packages were actually dependencies