# Project Preferences and Context

## User Preferences
- **Python Tooling**: Use `uv` for dependency management and script execution.
- **Editor**: Emacs.
- **Testing**: Prefer unit tests for routine development. Run integration tests only after significant changes or before a release. Once integration tests are functional, strongly consider creating unit test scenarios based on them using mock data.

## Test Data
- **GitHub Project**: https://github.com/users/ewilderj/projects/1

## Technical Decisions
- **Language**: Python.
- **Sync Direction**: Phase 1 is GitHub -> Org.

## Technical Context & Learnings
- **Integration Testing**:
  - Use persistent projects to avoid ID inflation.
  - `gh` CLI is insufficient for advanced Project V2 field management (e.g., adding options, renaming fields).
  - Use raw GraphQL mutations (`updateProjectV2Field`) for setting up complex test states (Custom Statuses).
- **Edge Cases**:
  - Draft issues and complex Markdown content require specific parsing logic, now covered by `tests/test_unit_scenarios.py`.
