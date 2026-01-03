# Emacs Minor Mode Ideas

This document captures potential enhancements for a `project-to-org-mode` minor mode that would make the GitHub Project sync experience more elegant and interactive.

## Visual Enhancements

### 1. Compact URL Display ✅
**Problem**: Full GitHub URLs are verbose and ugly:
```
:URL: https://github.com/ewilderj/hubber/issues/56
```

**Solution**: Render as a compact button showing `ewilderj/hubber#56`
- Click to open in browser
- `C-u` click to copy full URL
- Use `display` text properties to hide the full URL but keep it in the buffer
- Implementation: Use `font-lock` or `jit-lock` to add overlays/text properties

### 2. Inline Metadata Badges ✅
**Current**: Properties drawer takes up 8+ lines per item

**Proposed**: Show key info inline on the heading:
```
▼ DONE Book flight for Dallas  @ewilderj  [Todo]  🔗#56
```
- Assignees as `@username` tags
- Labels as colored badges
- Collapsible properties (just show `🔗#56` normally)
- Full properties available on demand

### 3. Status-aware Styling
- Color-code TODO keywords to match your GitHub project colors ✅
- Strikethrough for DONE items
- Dim excluded statuses
- Add emoji prefixes automatically (🟦 TODO, 🟨 STRT, ✅ DONE)
- Use `org-todo-keyword-faces` for color mapping ✅

### 4. Smart Folding ⭐
- Auto-fold properties drawers on file open
- Show one-line summary: `:PROPERTIES: [5 fields] #56 @ewilderj...`
- Unfold on hover or keypress
- Use `org-cycle-hook` or advise `org-cycle`

---

## Interactive Features

### 5. Quick Actions Menu
Key bindings for common operations:
- `C-c g o` - Open issue in GitHub (browser)
- `C-c g c` - Copy URL to clipboard
- `C-c g s` - Change status (with completion from project statuses)
- `C-c g @` - Assign/unassign users
- `C-c g r` - Refresh this item only (fetch latest from GitHub)
- `C-c g R` - Full project sync

### 6. Live Sync Indicator
- Modeline indicator: `[GH ✓ 2m ago]` or `[GH ⟳ syncing...]`
- Visual diff highlighting for items changed since last sync
- Non-intrusive notification on successful sync
- Warning if sync fails or file has conflicts

### 7. Inline Issue Preview
- Hover over `#56` to see issue title + first line of body in tooltip
- Or `C-c g p` to open preview in side window (using `display-buffer`)
- Could use GitHub API to fetch fresh data
- Cache previews to avoid rate limiting

---

## Structural Features

### 8. GitHub-aware Org Agenda
Extend `org-agenda` with GitHub-specific views:
- Filter agenda by GitHub assignee, label, or milestone
- Show items grouped by status (mirrors project board columns)
- Link to project board view in browser
- Custom agenda commands: `C-c a g` for "GitHub items"

### 9. Smart Completion
Enhanced editing experience:
- When adding new items, complete from open issues in the project
- Tab-complete assignee names from your GitHub org
- Label completion (from project labels)
- Status completion (from `#+GITHUB_STATUS_MAP`)

### 10. Diff Awareness (Phase 2)
For two-way sync:
- Highlight items modified locally with a marker (e.g., `*` or color)
- Warning icon if body was edited (can't sync back per design decision)
- Conflict markers for concurrent edits
- `C-c g d` to show diff with GitHub state

---

## Implementation Priorities

**Phase 1 (Quick Wins):**
1. Compact URLs (#1) - Immediate visual improvement
2. Inline metadata badges (#2) - Makes buffer much cleaner
3. Smart folding (#4) - Reduces visual clutter

**Phase 2 (Interactivity):**
4. Quick actions menu (#5) - Core workflow enhancement
5. Smart folding improvements (#4) - Context-aware behavior

**Phase 3 (Advanced):**
6. Live sync indicator (#6) - Status awareness
7. GitHub-aware agenda (#8) - Power user feature
8. Inline preview (#7) - Nice-to-have

**Phase 4 (Two-way sync support):**
9. Diff awareness (#10) - Required for Phase 2 of main project
10. Smart completion (#9) - Editing support

---

## Technical Notes

### Text Properties vs Overlays
- Use **text properties** for URL compacting (survives save/reload)
- Use **overlays** for dynamic state (sync status, conflicts) that shouldn't persist

### Overlay Display Techniques for Org-mode Folding

When adding visual elements (badges, icons, etc.) to org-mode headlines, the overlay positioning strategy matters significantly for fold behavior:

**Problem**: Org-mode folding uses the `invisible` text property. When a region becomes invisible:
- `after-string` overlays positioned at the end of headlines disappear (the string is tied to a position that becomes invisible when content is folded)
- `before-string` overlays on invisible text also disappear

**Solution**: Use the `display` property to **replace** visible text rather than append to it:

```elisp
;; DON'T: after-string at end of headline (disappears when folded)
(let ((ov (make-overlay headline-end headline-end)))
  (overlay-put ov 'after-string badge-text))

;; DO: display property on last character (stays visible when folded)
(let* ((last-char (buffer-substring-no-properties (1- headline-end) headline-end))
       (ov (make-overlay (1- headline-end) headline-end nil t)))
  (overlay-put ov 'display (concat last-char badge-text)))
```

**Why this works**: 
1. The overlay covers the **last character** of the headline text (which is always visible)
2. The `display` property replaces that character with itself + the badge
3. Since the character being replaced is in the visible headline, the badge stays visible regardless of fold state

**Key insight from Elisp manual**: "If the `display` property is a 'replacing' one, the `invisible` property of the same overlay will be ignored." This means `display` has special priority over visibility.

This technique was discovered by analyzing [org-tidy](https://github.com/jxq0/org-tidy), which uses `display` properties to hide/show drawer content independently of org-mode's folding mechanism.

### Performance Considerations
- Use `jit-lock` for on-demand fontification of large files
- Cache parsed property values
- Lazy-load GitHub API data (previews, completions)

### Compatibility
- Ensure graceful degradation when minor mode is off
- Don't break standard Org-mode functionality
- Test with common Org packages (org-roam, org-agenda, etc.)

---

## Future: Advanced Visualizations

### Burndown Charts in Emacs
- Use `orgtbl` or `ascii-art` to render simple charts
- Show velocity, items completed per day
- Integration with `org-super-agenda`

### Kanban View in Buffer
- Render project as columns (like GitHub's board view)
- Use `tabulated-list-mode` or custom rendering
- Drag-and-drop to change status (update `:STATUS:` property)

### Network Graph
- Visualize issue dependencies (if stored in properties)
- Use `graphviz-dot-mode` integration
- Show blockers and blocked-by relationships
