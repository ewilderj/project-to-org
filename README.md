# project-to-org

A tool to sync GitHub Projects to an Org-mode file.

## Features

*   **One-way Sync**: Fetches Issues and Draft Issues from a GitHub Project V2 and converts them to Org-mode headings.
*   **Status Mapping**: Maps project status (Todo, In Progress, Done) to Org TODO keywords.
*   **Properties**: Syncs all custom fields (Priority, Dates, Iteration, etc.) to the `:PROPERTIES:` drawer.
*   **Emacs Integration**: Provides an interactive command `project-to-org-sync` to sync directly from Emacs.

## Installation

### Python Dependencies

This project uses `uv` for dependency management.

```bash
# Install dependencies
uv sync
```

### Emacs Setup

Add the following to your Emacs configuration:

```elisp
(add-to-list 'load-path "/path/to/project-to-org")
(require 'project-to-org)
```

## Usage

### 1. Prepare your Org file

Create an Org file and add the `#+GITHUB_PROJECT_URL` property at the top:

```org
#+GITHUB_PROJECT_URL: https://github.com/users/YOUR_USERNAME/projects/YOUR_PROJECT_NUMBER
```

### 2. Run the Sync

**From Emacs:**

Run `M-x project-to-org-sync`.

**From Command Line:**

```bash
uv run src/project_to_org/main.py --project-url https://github.com/users/ewilderj/projects/1 --org-file my-project.org
```

## Configuration

You can customize the Python command in Emacs:

```elisp
(setq project-to-org-python-command "uv run")
```
