# project-to-org

View your GitHub Projects as Org-mode files with rich visual enhancements.

> ⚠️ **Note**: This is currently a **one-way sync** (GitHub → Org). Changes made in your Org file will be overwritten on the next sync. Use this tool for viewing and organizing your GitHub Projects in Emacs, not for editing them.

## What it Does

project-to-org fetches issues and draft issues from a GitHub Project V2 and converts them to an Org-mode file. The accompanying Emacs minor mode adds visual enhancements like status colors, compact URLs, and inline metadata badges.

![Screenshot](docs/screenshot.png)

(Screenshot notes: theme Dracula, with additional modes `mixed-pitch-mode`, `org-modern` and `org-tidy`)

## Features

### Data Sync
- **Issues & Draft Issues**: All items from your GitHub Project become Org headings
- **Status → TODO**: Project status maps to Org TODO keywords (e.g., "In Progress" → `STRT`)
- **Custom Fields**: Priority, dates, iteration, and other fields sync to `:PROPERTIES:` drawers
- **Assignees & Labels**: Stored as properties and displayed as inline badges

### Visual Enhancements (Emacs)
- **Status Colors**: TODO keywords are colored to match GitHub's status colors (GRAY, BLUE, GREEN, etc.)
- **Compact URLs**: Long GitHub URLs display as `owner/repo#123` (click to open, middle-click to copy)
- **Inline Badges**: Issue numbers, assignees, and labels appear as badges on each heading
- **Auto-fold Properties**: Properties drawers are automatically folded for cleaner viewing

## Requirements

- **Python 3.10+** with [uv](https://github.com/astral-sh/uv) for dependency management
- **GitHub CLI** (`gh`) authenticated with access to your project
- **Emacs 29.1+** for the visual enhancements (optional but recommended)

## Installation

### Emacs (Recommended)

The Emacs package handles everything—it downloads the Python script and runs syncs for you.

#### Using use-package with `:vc` (Emacs 29+)

```elisp
(use-package project-to-org
  :vc (:url "https://github.com/ewilderj/project-to-org"
       :rev :newest)
  :commands (project-to-org-sync project-to-org-mode))
```

#### Using straight.el

```elisp
(use-package project-to-org
  :straight (:host github :repo "ewilderj/project-to-org")
  :commands (project-to-org-sync project-to-org-mode))
```

#### Using quelpa

```elisp
(use-package project-to-org
  :quelpa (project-to-org :fetcher github :repo "ewilderj/project-to-org")
  :commands (project-to-org-sync project-to-org-mode))
```

### CLI Only

If you just want the Python script without Emacs integration:

```bash
git clone https://github.com/ewilderj/project-to-org.git
cd project-to-org
uv run project_to_org.py --help
```

## Quick Start

### 1. Create an Org file with the project URL

```org
#+GITHUB_PROJECT_URL: https://github.com/users/YOUR_USERNAME/projects/1
```

You can find your project URL by opening your GitHub Project in a browser and copying the URL.

### 2. Sync from Emacs

Open the Org file and run:

```
M-x project-to-org-sync
```

The file will be populated with your project's issues. The minor mode (`project-to-org-mode`) enables automatically via file-local variables added by the sync.

### Alternative: Sync from Command Line

```bash
uv run project_to_org.py \
  --project-url https://github.com/users/YOUR_USERNAME/projects/1 \
  --org-file my-project.org
```

## Configuration

### Emacs Customization

All settings are in the `project-to-org` customization group (`M-x customize-group RET project-to-org`).

| Variable | Default | Description |
|----------|---------|-------------|
| `project-to-org-python-command` | `"uv run"` | Command to run Python scripts |
| `project-to-org-script-path` | `"project_to_org.py"` | Script path (relative or absolute) |
| `project-to-org-compact-urls` | `t` | Show URLs as `owner/repo#123` |
| `project-to-org-inline-metadata` | `t` | Show badges on headings |
| `project-to-org-fold-properties` | `t` | Auto-fold properties drawers |
| `project-to-org-issue-prefix` | `"#"` | Prefix for issue badges |
| `project-to-org-assignee-prefix` | `"👤 "` | Prefix for assignee badges |
| `project-to-org-label-prefix` | `"🏷️ "` | Prefix for label badges |

### Customizing Status and Priority Mapping

After the first sync, the Org file contains headers that control how GitHub fields map to Org. You can edit these directly:

```org
#+GITHUB_STATUS_MAP: Todo=TODO "In Progress"=STRT Done=DONE
#+GITHUB_PRIORITY_MAP: Low=C Medium=B High=A
#+GITHUB_EXCLUDE_STATUSES: Done
```

- **Status Map**: Maps GitHub status names to Org TODO keywords. Quote names with spaces.
- **Priority Map**: Maps GitHub priority values to Org priority cookies (`[#A]`, `[#B]`, etc.).
- **Exclude Statuses**: Space-separated list of statuses to omit from the Org file.

Your customizations are preserved on re-sync. The tool auto-detects common priority schemes (P0/P1/P2, High/Medium/Low) so you often won't need to configure anything.

The same mappings can be set via CLI options (`--status-map`, `--priority-map`, `--exclude-statuses`) for initial setup.

## How It Works

1. The standalone Python script (`project_to_org.py`) has inline dependencies and runs via `uv run`
2. It queries GitHub's GraphQL API via the `gh` CLI for authentication
3. Issues and draft issues are converted to Org headings with properties
4. Status colors are stored in `#+GITHUB_STATUS_COLORS` for the Emacs mode
5. The Emacs mode applies overlays for colors, compact URLs, and badges
6. Overlays refresh automatically when the file is saved

## Limitations

- **One-way sync only**: GitHub → Org. Local changes will be lost on re-sync.
- **No write-back**: Editing TODO states in Org does not update GitHub.
- **100 item limit**: Currently fetches the first 100 items from a project (pagination not yet implemented).

## License

MIT
