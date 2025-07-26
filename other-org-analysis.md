# Analysis of other.org Contents

## 1. Special Files Configuration (lines 152-172)
**Status**: Keep but simplify
- Defines many specific org files (gtd.org, journal.org, habits.org, etc.)
- Some may no longer be used (beorg mobile sync, diet journal)
- **Recommendation**: Move essential files to config.org, remove unused ones

## 2. Advanced Speed Commands (lines 84-150)
**Status**: Review and streamline
- Custom speed commands for navigation and editing
- Many set to 'ignore' (disabled)
- **Recommendation**: Keep only actively used commands, migrate to config.org

## 3. GTD Helper Functions (lines 192-520)
**Status**: Complex - needs careful review
- Project/task predicates (bh/is-project-p, bh/is-task-p, etc.)
- Skip functions for agenda views
- Bernt Hansen's GTD implementation
- **Recommendation**: Simplify or replace with org-super-agenda filters

## 4. Custom Agenda Commands (lines 539-701)
**Status**: Very complex - simplify
- Multiple custom agenda views (Clarify, GTD, etc.)
- Many skip functions and custom sorting
- **Recommendation**: Replace with simpler org-super-agenda configuration

## 5. Capture Templates (lines 734-795)
**Status**: Keep selectively
- Extended templates: 5-minute journal, food diary, meetings
- Auto habit tracking with hardcoded IDs
- **Recommendation**: Keep useful templates, remove hardcoded IDs

## 6. Clock Settings (lines 707-732)
**Status**: Keep
- Pomodoro support
- Clock persistence
- Time logging settings
- **Recommendation**: Move to config.org as-is

## 7. Export Settings (lines 857-873)
**Status**: Review
- HTML postamble configuration
- Basic export setup
- **Recommendation**: Merge with export config in config.org

## 8. Non-Org Sections to Remove:
- Helm configuration (lines 875-910) - Already removed Helm
- Navigation/Windmove (lines 911-984) - Duplicate
- Terminal configuration (lines 985-1011) - Move if needed
- Compilation settings (lines 1012-1041) - Already in config.org
- Tree-sitter config (lines 1043-1116) - Already migrated
- Web/C++ modes (lines 1117-1171) - Review separately

## Summary Recommendations:

### Immediate Actions:
1. Move clock settings to config.org
2. Extract useful capture templates
3. Simplify file definitions

### Requires Decision:
1. GTD workflow functions - keep or replace?
2. Complex agenda commands - simplify with org-super-agenda?
3. Speed commands - which ones do you actually use?

### Can Remove:
1. All non-org sections (Helm, duplicate navigation, etc.)
2. Hardcoded habit IDs
3. Unused file definitions