# GTD Simplification Project - Complete Implementation Guide

## Critical Discoveries and Nuances

### 1. Tag System Complexity

**Functional Tags:**
- `BUCKET` - Marks valid refile targets (buckets for organizing)
- `PROJECTS` - Parent tag for projects (note: plural, not singular)
- `REFILE` - Marks items in inbox needing processing

**Context Tags:**
- Location contexts: `@work`, `@home`, `@computer`, `@anywhere`, `@errands`
- Personal contexts: `@fish`, `@sag`, `@woodshop`
- Type tags: `#measure`, `#reachout`, `#tools`, `#review`

**Category-based Filtering:**
- `CATEGORY="Someday"` - Someday/maybe items
- `CATEGORY="Tickler"` - Future-dated items
- `CATEGORY="Read Me"` - Reading list

### 2. Inheritance Behavior
- `BUCKET` and `PROJECTS` tags don't inherit to subtasks
- This prevents subtasks from appearing as refile targets

### 3. TODO State Triggers
Your config automatically adds/removes tags based on TODO state changes:
- CANCELLED → adds CANCELLED tag
- WAITING → adds WAITING tag
- HOLD → adds HOLD tag, removes WAITING
- TODO/NEXT → removes all status tags

### 4. Special Functions/Variables
- `bh/hide-scheduled-and-waiting-next-tasks` - Toggle for agenda filtering
- `org-todo-yesterday` - Custom function for backdating completions
- Special keybindings in agenda mode (M-., C-., Y)

## Complete Implementation Plan

### Phase 1: Preparation and Analysis

1. **Audit Current GTD Structure**
   ```bash
   # Find all headings with TODO keywords in gtd.org
   grep -n "^\*\+ \(TODO\|NEXT\|DONE\|WAITING\|HOLD\|CANCELLED\)" ~/org/gtd.org
   
   # Find all tags currently in use
   grep -o ":[^:]*:" ~/org/gtd.org | sort | uniq
   
   # Find CATEGORY properties
   grep -n ":CATEGORY:" ~/org/gtd.org
   ```

2. **Backup Current State**
   ```bash
   cp ~/org/gtd.org ~/org/gtd.org.backup-$(date +%Y%m%d)
   cp ~/.emacs.d/other.org ~/.emacs.d/other.org.backup-$(date +%Y%m%d)
   ```

### Phase 2: Project Identification Strategy

**Decision Point: Choose ONE approach**

**Option A: Tag-based (Recommended)**
```elisp
;; Add to config.org
(defun my/mark-as-project ()
  "Mark current heading as a project."
  (interactive)
  (org-toggle-tag "PROJECT"))

;; Update tag inheritance
(setq org-tags-exclude-from-inheritance 
      '("BUCKET" "PROJECTS" "PROJECT"))  ; Note: Add PROJECT
```

**Option B: Property-based**
```elisp
(defun my/mark-as-project ()
  "Mark current heading as a project."
  (interactive)
  (org-set-property "Type" "project"))
```

**Migration Script for Existing Projects:**
```elisp
(defun my/migrate-projects-to-tags ()
  "Find all existing projects and tag them."
  (interactive)
  (org-map-entries
   (lambda ()
     (when (and (member (org-get-todo-state) org-todo-keywords-1)
                (save-excursion
                  (org-goto-first-child)))
       (org-toggle-tag "PROJECT" 'on)))
   nil 'file))
```

### Phase 3: Enhanced org-super-agenda Configuration

```elisp
;; Replace simple config in config.org with:
(use-package org-super-agenda
  :ensure t
  :after org-agenda
  :config
  (org-super-agenda-mode)
  
  ;; Custom transformers
  (defun my/org-super-agenda-transformer-project (item)
    "Add [P] to project items."
    (when (org-super-agenda--item-has-tag-p item "PROJECT")
      (concat "[P] " item)))
  
  :custom
  ;; Global groups for all agenda views
  (org-super-agenda-groups
   '((:name "⚡ Critical"
            :and (:deadline past :not (:todo "DONE"))
            :face (:foreground "red" :weight bold))
     (:name "📌 Stuck Projects"
            :and (:tag "PROJECT" 
                  :not (:children ("NEXT" "WAITING")))
            :face (:foreground "orange"))
     (:name "📅 Today"
            :time-grid t
            :and (:scheduled today
                  :not (:todo "DONE")))
     (:name "🎯 Focus (Next Actions)"
            :todo "NEXT"
            :not (:tag "PROJECT"))
     (:name "📥 Inbox"
            :tag "REFILE")
     (:name "⏸️ Waiting For"
            :todo "WAITING")
     (:name "🗂️ Active Projects"
            :and (:tag "PROJECT"
                  :children ("NEXT" "TODO"))
            :transformer my/org-super-agenda-transformer-project)
     (:name "📚 Someday/Maybe"
            :category "Someday"
            :order 99)
     (:auto-tags t))))
```

### Phase 4: New Agenda Commands

```elisp
(setq org-agenda-custom-commands
      '(;; Daily Dashboard - replaces "j" GTD view
        ("d" "Daily Dashboard"
         ((agenda "" 
                  ((org-agenda-span 'day)
                   (org-super-agenda-groups
                    '((:name "Schedule"
                             :time-grid t)
                      (:discard (:anything))))))
          (alltodo ""
                   ((org-agenda-overriding-header "")
                    (org-super-agenda-groups
                     '((:name "📥 Inbox - Process These"
                              :tag "REFILE"
                              :order 1)
                       (:name "🚨 Stuck Projects"
                              :and (:tag "PROJECT"
                                    :not (:children ("NEXT" "WAITING")))
                              :order 2)
                       (:name "🎯 Next Actions"
                              :todo "NEXT"
                              :not (:tag "PROJECT")
                              :order 3)
                       (:name "⏸️ Waiting For"
                              :todo "WAITING"
                              :order 7)
                       (:discard (:tag "PROJECT"))
                       (:discard (:category ("Someday" "Tickler")))
                       (:discard (:scheduled future))
                       (:discard (:deadline future))))))))
        
        ;; Context-based view - replaces "a" Act view
        ("c" "Context Actions"
         ((alltodo ""
                   ((org-agenda-overriding-header "Actions by Context")
                    (org-agenda-sorting-strategy '(tag-up priority-down))
                    (org-super-agenda-groups
                     '((:name "🏠 Home"
                              :and (:tag "@home" :todo "NEXT"))
                       (:name "💼 Work"
                              :and (:tag "@work" :todo "NEXT"))
                       (:name "💻 Computer"
                              :and (:tag "@computer" :todo "NEXT"))
                       (:name "🚗 Errands"
                              :and (:tag "@errands" :todo "NEXT"))
                       (:name "🐟 Fish"
                              :and (:tag "@fish" :todo "NEXT"))
                       (:name "🏛️ SAG"
                              :and (:tag "@sag" :todo "NEXT"))
                       (:name "🔨 Woodshop"
                              :and (:tag "@woodshop" :todo "NEXT"))
                       (:name "📍 Anywhere"
                              :and (:tag "@anywhere" :todo "NEXT"))
                       (:name "❓ No Context"
                              :and (:todo "NEXT"
                                    :not (:regexp "@")))
                       (:discard (:anything))))))))
        
        ;; Weekly Review
        ("w" "Weekly Review"
         ((agenda ""
                  ((org-agenda-span 'week)
                   (org-agenda-start-day "-1d")
                   (org-agenda-start-on-weekday 1)))
          (tags "PROJECT"
                ((org-agenda-overriding-header "Project Review")
                 (org-super-agenda-groups
                  '((:name "🚨 Stuck - Need Next Actions"
                           :not (:children ("NEXT" "WAITING")))
                    (:name "✅ Active"
                           :children "NEXT")
                    (:name "⏸️ On Hold"
                           :children "WAITING")))))
          (todo "WAITING"
                ((org-agenda-overriding-header "Waiting For - Follow Up?")))
          (tags "DELEGATED"
                ((org-agenda-overriding-header "Delegated - Check Status")))))
        
        ;; Process/Clarify - replaces "C" Clarify
        ("p" "Process"
         ((tags "REFILE"
                ((org-agenda-overriding-header "📥 Inbox Items to Process")))
          (tags "-{.*}+TODO=\"TODO\""
                ((org-agenda-overriding-header "🏷️ Tasks Without Context")
                 (org-agenda-skip-function
                  '(org-agenda-skip-entry-if 'nottodo '("TODO" "NEXT")))))))
        
        ;; Reading List
        ("r" "Reading"
         ((tags "^review"
                ((org-agenda-overriding-header "📚 Reading/Review List")))
          (tags-todo "CATEGORY=\"Read Me\""
                     ((org-agenda-overriding-header "📖 Read Me Items")))))))
```

### Phase 5: Preserve Special Behaviors

```elisp
;; Add to config.org after org-super-agenda config

;; Preserve agenda mode keybindings
(add-hook 'org-agenda-mode-hook
          (lambda ()
            (define-key org-agenda-mode-map (kbd "M-.") 
                        (lambda () (interactive) (org-agenda-schedule nil ".")))
            (define-key org-agenda-mode-map (kbd "C-.") 
                        (lambda () (interactive) (org-agenda-schedule t)))
            (define-key org-agenda-mode-map "Y" 'org-todo-yesterday)
            (define-key org-agenda-mode-map "N" nil)
            (cd "/tmp")
            (add-hook 'auto-save-hook 'org-save-all-org-buffers nil t)
            (auto-save-mode)))

;; Toggle for showing/hiding scheduled NEXT tasks
(defvar my/hide-scheduled-next-tasks t
  "Hide scheduled and waiting NEXT tasks in agenda views.")

(defun my/toggle-next-task-display ()
  "Toggle display of scheduled/waiting NEXT tasks."
  (interactive)
  (setq my/hide-scheduled-next-tasks (not my/hide-scheduled-next-tasks))
  (when (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s scheduled NEXT tasks" 
           (if my/hide-scheduled-next-tasks "Hiding" "Showing")))
```

### Phase 6: Migration Checklist

1. **Pre-migration Tests**
   - [ ] Count current projects: `grep -c "has subtasks" in agenda`
   - [ ] Count stuck projects in "C" view
   - [ ] Count NEXT actions in "j" view
   - [ ] Note any DELEGATED items

2. **Migration Steps**
   - [ ] Apply PROJECT tags to all projects using migration function
   - [ ] Verify BUCKET tags on refile targets
   - [ ] Check for orphaned PROJECTS (plural) tags
   - [ ] Ensure all tasks have context tags

3. **Post-migration Validation**
   - [ ] All projects appear in project review
   - [ ] Stuck projects correctly identified
   - [ ] Context view shows all NEXT actions
   - [ ] Inbox items appear in process view

### Phase 7: Cleanup other.org

**Safe to Remove:**
- Lines 187-474: All predicate and skip functions
- Lines 519-653: Complex agenda commands
- Line 9: PROJECTS tag exclusion (if not using plural form)

**Must Preserve:**
- Lines 8-35: Environment setup and save functions
- Lines 38-58: Tag definitions (unless moving to config.org)
- Lines 70-81: Random journal entry function
- Lines 83-147: Speed commands (review separately)
- Lines 149-176: File definitions and refile config

### Phase 8: Edge Cases and Gotchas

1. **DELEGATED vs WAITING**
   - Current system has both
   - DELEGATED gets special agenda section
   - Decision: Keep both or merge into WAITING with note?

2. **Category-based Filtering**
   - Someday/Tickler categories filter out of most views
   - Need to preserve this behavior in super-agenda

3. **"Read Me" Category**
   - Special category for reading list
   - Filtered out of NEXT actions
   - Preserve with dedicated agenda command

4. **Refile Target Filtering**
   - Uses BUCKET tag OR is-project predicate
   - Replace with: BUCKET tag OR PROJECT tag

5. **Speed Commands**
   - Many custom shortcuts that might be muscle memory
   - Review each before removing

## Current GTD Workflow (for reference)

### Capture
- `C-c c t` → captures to Inbox in gtd.org

### Clarify (current "C" agenda)
- Shows: REFILE tasks, stuck projects, tasks without location
- Process: add context, set TODO/NEXT, refile

### Act (current "a" agenda)
- Shows NEXT actions grouped by context
- Filters out scheduled/deadlined items

### GTD View (current "j" agenda)
- Today's agenda
- All NEXT actions
- Projects list
- Waiting items
- Stuck projects
- Delegated items

### Key Bindings in Agenda
- `M-.` → Schedule for today
- `C-.` → Remove schedule
- `Y` → Mark done yesterday
- `N` → Disabled (was for adding notes)

## Testing Strategy

1. Run old and new agendas side-by-side
2. Compare counts of:
   - Total projects
   - Stuck projects
   - NEXT actions per context
   - WAITING items
   - Items in inbox

3. Test workflows:
   - Capture → Process → Refile
   - Mark project stuck/unstuck
   - Context switching
   - Weekly review process

## Rollback Plan

If issues arise:
1. Comment out new agenda commands
2. Uncomment old agenda commands in other.org
3. Remove PROJECT tags if added
4. Restore from backups if needed

This preserves all current functionality while dramatically simplifying the implementation.