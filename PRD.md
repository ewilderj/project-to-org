# Product Requirement Document: GitHub Projects <-> Org-mode Sync

## 1. Introduction
This document outlines the requirements for a tool that enables two-way synchronization between GitHub Projects and an Emacs Org-mode text file. The tool aims to bridge the gap between the web-based project management of GitHub and the text-based, computable environment of Org-mode.

## 2. Problem Statement
GitHub Projects contain vital information about development state, but this data is often isolated from other organizational context (OKRs, org charts, etc.) and requires a web interface that can be cumbersome for power users who prefer text-based workflows in Emacs.

## 3. Goals
*   **Data Portability**: Make GitHub Project data accessible in a plain text format (Org-mode) to allow for further computation and linking.
*   **Workflow Efficiency**: Enable users to view and edit project items directly within Emacs.
*   **Synchronization**: Ensure data consistency between the local Org file and the remote GitHub Project.

## 4. Phased Approach

### Phase 1: One-Way Sync (GitHub Project -> Org file)
*   **Tech Stack**: Python.
*   Fetch items (Issues and Draft Issues) from a specified GitHub Project.
*   Convert project items into Org-mode headings.
*   Map "Status" field to Org TODO keywords.
*   Map all other fields to the `:PROPERTIES:` drawer.
*   Overwrite or update a local Org file with the remote state.

### Phase 1.5: Emacs Integration
*   Create an Elisp wrapper to call the Python script.
*   Bind the sync command to an interactive function (e.g., `M-x gh-project-sync`).

### Phase 2: Two-Way Sync
*   Detect changes in the local Org file.
*   Push updates (status changes, text edits) back to GitHub.
*   Handle creation of new items in Org-mode and pushing them to GitHub.

### Phase 3: Conflict Resolution & Robustness
*   Handle concurrent modifications (async changes on GitHub vs. local edits).
*   Implement conflict resolution strategies (e.g., "last modified wins", "manual merge", or specific field-level rules).
*   Error handling for network issues or API rate limits.
 Field** -> **Org TODO Keyword** (e.g., "Todo" -> TODO, "Done" -> DONE).
*   **All Custom Fields** -> **:PROPERTIES: Drawer** (including Iteration, Dates, Single Selects).
*   **Assignees** -> **Property** (e.g., `:ASSIGNEES: user1, user2`).
*   **Labels** -> **Org Tags** (or Property if tags are too complex).

### 5.2 Configuration
*   Configuration will be stored as **File-Level Properties** at the top of the Org file.
    *   `#+GITHUB_PROJECT_URL: https://github.com/orgs/my-org/projects/1`
    *   `#+GITHUB_SYNC_METADATA: ...` (for storing sync state/cursors).
*   Authentication via environment variables or standard GitHub CLI auth (gh auth login) if possible, or .env file.

### 5.3 Scope
*   **Included**: Issues, Draft Issues.
*   **Excluded (for now)**: Pull Requests.

## 6. Technical Decisions

### 6.1 Data Structure & Mapping
*   **Status Mapping**:
    *   **Heuristics**: We prioritize a 1:1 (injective) mapping to ensure unambiguous two-way sync.
    *   **Defaults**: Common statuses map to standard Org keywords (`Todo` -> `TODO`, `In Progress` -> `STRT`, `Done` -> `DONE`, `Someday` -> `WAIT`).
    *   **Fallback**: Unknown statuses are converted to uppercase with underscores (e.g., "Needs Review" -> `NEEDS_REVIEW`).
    *   **Collision Handling**: If two statuses map to the same keyword, we append a suffix (e.g., `REVIEW_1`) to maintain uniqueness.
*   **Field Normalization**:
    *   GitHub Field names are converted to valid Org Property keys by uppercasing and replacing spaces with underscores (e.g., "Target Date" -> `:TARGET_DATE:`).
    *   **Labels & Assignees**: Currently stored as comma-separated values in `:LABELS:` and `:ASSIGNEES:` properties, rather than Org tags, to keep the headline clean.

### 6.2 Persistence & Configuration
*   **File-Level Metadata**: We store sync configuration directly in the Org file header to make the file self-contained.
    *   `#+GITHUB_PROJECT_URL`: The source of truth for the sync target.
    *   `#+GITHUB_STATUS_MAP`: A serialized string of the status mapping used. This allows the user to customize the mapping in the file and have the tool respect it (planned for Phase 1.5/2).
    *   `#+GITHUB_EXCLUDE_STATUSES`: Persists the list of ignored statuses.
*   **Dynamic TODO Keywords**: The `#+TODO` line is dynamically generated based on the active status map, ensuring that cycling TODO states in Emacs matches the available GitHub statuses.

### 6.3 Language & Tooling
*   **Language**: Python for the core logic (using `requests` or a GraphQL client).
*   **Integration**: Emacs Lisp (Elisp) will eventually wrap the Python tool.
*   **Trigger**: Manual execution (CLI first, then Elisp command). Project Number.
*   Authentication handling (PAT/OAuth).

## 7. Testing Strategy

### 7.1 Philosophy
We employ a hybrid testing strategy to balance speed and confidence:
*   **Unit Tests (`tests/test_unit_scenarios.py`, `tests/test_sync.py`)**: The primary driver for development. Fast, mocked tests that cover logic, edge cases, and content parsing.
*   **Integration Tests (`tests/test_integration.py`, `tests/test_integration_edge_cases.py`)**: The safety net. Slow, expensive tests that run against a live GitHub sandbox. These are run only after significant changes or before a release.

### 7.2 Test Suites

#### Unit Tests
*   **Scenarios**: Covers complex content parsing (Markdown tables, code blocks), Draft Issues, and Empty Projects.
*   **Logic**: Verifies status mapping heuristics, collision handling, and Org file generation.
*   **Mechanism**: Mocks the JSON output from the `gh` CLI to simulate various project states without network calls.

#### Integration Tests
*   **Environment**: Runs against a live GitHub repository (`ewilderj/project-to-org-sandbox`).
*   **Mechanism**: Uses a **persistent GitHub Project** to avoid resource exhaustion and ID inflation.
*   **Tooling**:
    *   Uses `gh` CLI for basic item management.
    *   Uses **raw GraphQL mutations** (`updateProjectV2Field`) for complex setup (e.g., renaming fields, adding custom status options) where the CLI is insufficient.
*   **Coverage**:
    *   **Happy Path**: Full sync loop (Create Issue -> Sync -> Verify).
    *   **Edge Cases**: Custom Statuses, Status Exclusion, Draft Issues.

### 7.3 Implemented Scenarios
The following scenarios are fully covered by our test suite:

#### A. Status Mapping & Filtering
*   **Custom Statuses**: Verified via integration tests using GraphQL to inject non-standard statuses (e.g., "Triage", "On Hold").
*   **Status Exclusion**: Verified that `#+GITHUB_EXCLUDE_STATUSES` correctly omits items.
*   **Status Collision**: Verified that heuristics generate unique keywords.

#### B. Content Handling
*   **Complex Content**: Verified via unit tests (parsing Markdown tables, code blocks) and integration tests.
*   **Draft Issues**: Verified handling of `DRAFT_ISSUE` type and ID differences.

#### C. Project Structure
*   **Empty Project**: Verified graceful handling of projects with 0 items.

## 8. Open Questions / To Be Defined
*   **Tech Stack**: Preferred language for the sync tool (Python, Go, Elisp, etc.)?
*   **Sync Trigger**: Manual command, file watcher, or cron?
*   **Org Structure**: Specific hierarchy preferences (flat list vs. grouped by status)?
*   **Conflict Strategy**: Default behavior for Phase 3?
