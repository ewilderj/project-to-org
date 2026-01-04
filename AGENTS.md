# Project Preferences and Context

## User Preferences
- **Python Tooling**: Use `uv` for dependency management and script execution.
- **Elisp Tooling**: Use `eask` for Emacs Lisp development (compile, lint, test).
- **Editor**: Emacs.
- **Testing**: Prefer unit tests for routine development. Run integration tests only after significant changes or before a release. Once integration tests are functional, strongly consider creating unit test scenarios based on them using mock data.

## Test Data
- **GitHub Project**: https://github.com/users/ewilderj/projects/1

## Technical Decisions
- **Language**: Python.
- **Sync Direction**: Phase 1 is GitHub -> Org.

## Elisp Development with Eask
After editing `project-to-org.el`, always validate with eask before asking the user to reload:

```bash
# Format code consistently
eask fmt elisp-autofmt project-to-org.el

# Compile and check for warnings (replaces check_parens.py)
eask compile

# Check docstrings
eask lint checkdoc

# Check package conventions
eask lint package
```

Common issues eask catches:
- Unbalanced parentheses (compile will fail)
- Obsolete functions (suggests modern replacements)
- Unused lexical arguments (prefix with `_`)
- Trailing whitespace
- Missing dependency declarations

Install dev dependencies with: `eask install-deps --dev`

## Technical Context & Learnings
- **Integration Testing**:
  - Use persistent projects to avoid ID inflation.
  - `gh` CLI is insufficient for advanced Project V2 field management (e.g., adding options, renaming fields).
  - Use raw GraphQL mutations (`updateProjectV2Field`) for setting up complex test states (Custom Statuses).
- **Edge Cases**:
  - Draft issues and complex Markdown content require specific parsing logic, now covered by `tests/test_unit_scenarios.py`.
- **Elisp Overlay Approach**:
  - Metadata badges use `'display` property overlays on the last character of headlines.
  - **Do NOT change to `after-string`** - the `display` approach is required for correct folding behavior when headlines are collapsed.
