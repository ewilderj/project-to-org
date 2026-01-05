#!/usr/bin/env python3
# /// script
# requires-python = ">=3.10"
# dependencies = [
#     "requests>=2.31.0",
# ]
# ///
"""
Sync GitHub Projects to Org-mode.

Usage:
    uv run project-to-org.py --project-url https://github.com/users/USERNAME/projects/1 --org-file output.org
"""

import argparse
import datetime
import os
import re
import shlex
import subprocess
import sys

import requests


# =============================================================================
# GitHub Client
# =============================================================================

def get_github_token():
    """Retrieve GitHub token from environment or gh CLI."""
    token = os.getenv("GITHUB_TOKEN")
    if token:
        return token

    try:
        result = subprocess.run(
            ["gh", "auth", "token"],
            capture_output=True,
            text=True,
            check=True
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        raise RuntimeError(
            "Could not retrieve GitHub token. "
            "Please set GITHUB_TOKEN or login with `gh auth login`."
        )
    except FileNotFoundError:
        raise RuntimeError(
            "`gh` CLI not found. Please install it or set GITHUB_TOKEN."
        )


def parse_project_url(url):
    """Parse GitHub Project URL to get owner type, owner name, and project number."""
    # Matches https://github.com/users/USERNAME/projects/NUMBER
    user_match = re.match(r"https://github\.com/users/([^/]+)/projects/(\d+)", url)
    if user_match:
        return "user", user_match.group(1), int(user_match.group(2))

    # Matches https://github.com/orgs/ORGNAME/projects/NUMBER
    org_match = re.match(r"https://github\.com/orgs/([^/]+)/projects/(\d+)", url)
    if org_match:
        return "organization", org_match.group(1), int(org_match.group(2))

    raise ValueError(f"Invalid GitHub Project URL: {url}")


def fetch_project_items(owner_type, owner, number, token):
    """Fetch items from a GitHub Project V2."""
    url = "https://api.github.com/graphql"
    headers = {"Authorization": f"Bearer {token}"}

    # Construct the query based on owner type
    if owner_type == "user":
        owner_query_part = f'user(login: "{owner}")'
    else:
        owner_query_part = f'organization(login: "{owner}")'

    query = f"""
    query {{
      {owner_query_part} {{
        projectV2(number: {number}) {{
          title
          fields(first: 20) {{
            nodes {{
              ... on ProjectV2FieldCommon {{
                name
                dataType
              }}
              ... on ProjectV2SingleSelectField {{
                name
                options {{
                  name
                  color
                }}
              }}
            }}
          }}
          items(first: 100) {{
            nodes {{
              id
              type
              content {{
                ... on Issue {{
                  title
                  body
                  state
                  number
                  url
                  assignees(first: 10) {{
                    nodes {{
                      login
                    }}
                  }}
                  labels(first: 10) {{
                    nodes {{
                      name
                    }}
                  }}
                }}
                ... on DraftIssue {{
                  title
                  body
                }}
              }}
              fieldValues(first: 20) {{
                nodes {{
                  ... on ProjectV2ItemFieldSingleSelectValue {{
                    name
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                  ... on ProjectV2ItemFieldTextValue {{
                    text
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                  ... on ProjectV2ItemFieldDateValue {{
                    date
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                  ... on ProjectV2ItemFieldIterationValue {{
                    title
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                }}
              }}
            }}
          }}
        }}
      }}
    }}
    """

    response = requests.post(url, json={"query": query}, headers=headers)
    if response.status_code != 200:
        raise RuntimeError(
            f"GraphQL query failed: {response.status_code} {response.text}"
        )

    data = response.json()
    if "errors" in data:
        raise RuntimeError(f"GraphQL errors: {data['errors']}")

    # Navigate the response structure dynamically based on owner_type
    root = (
        data["data"]["user"]
        if owner_type == "user"
        else data["data"]["organization"]
    )
    if not root or not root["projectV2"]:
        raise RuntimeError(f"Project not found: {owner}/{number}")

    return root["projectV2"]


# =============================================================================
# Org Converter
# =============================================================================

def extract_config_from_org(file_path):
    """Extract configuration from an existing Org file."""
    config = {}
    if not os.path.exists(file_path):
        return config

    with open(file_path, 'r') as f:
        for line in f:
            if line.startswith("#+GITHUB_STATUS_MAP:"):
                config['status_map'] = line.split(":", 1)[1].strip()
            elif line.startswith("#+GITHUB_EXCLUDE_STATUSES:"):
                config['exclude_statuses'] = line.split(":", 1)[1].strip().split()
            elif line.startswith("#+GITHUB_PRIORITY_MAP:"):
                config['priority_map'] = line.split(":", 1)[1].strip()
            elif line.startswith("#+GITHUB_STATUS_COLORS:"):
                config['status_colors'] = line.split(":", 1)[1].strip()
    return config


class OrgConverter:
    def __init__(
        self,
        project_data,
        project_url=None,
        exclude_statuses=None,
        status_map_str=None,
        priority_map_str=None,
        status_colors_str=None,
        add_local_variables=True
    ):
        self.project_data = project_data
        self.project_url = project_url
        self.exclude_statuses = exclude_statuses or []
        self.add_local_variables = add_local_variables

        if status_map_str:
            self.status_map = self._parse_status_map(status_map_str)
            self.status_map_str = status_map_str
        else:
            self.status_map = self._generate_default_status_map()
            self.status_map_str = self._generate_status_map_str()

        self._validate_status_map()

        # Priority mapping
        if priority_map_str:
            self.priority_map = self._parse_priority_map(priority_map_str)
            self.priority_map_str = priority_map_str
            self.priority_scheme = self._detect_priority_scheme_from_map()
        else:
            self.priority_map, self.priority_scheme = self._generate_default_priority_map()
            self.priority_map_str = (
                self._generate_priority_map_str() if self.priority_map else None
            )

        # Status colors (GitHub status -> org keyword -> color)
        if status_colors_str:
            self.status_colors = self._parse_status_colors(status_colors_str)
            self.status_colors_str = status_colors_str
        else:
            self.status_colors = self._extract_status_colors()
            self.status_colors_str = (
                self._generate_status_colors_str() if self.status_colors else None
            )

    def _generate_default_status_map(self):
        """Generate a default status map based on project fields."""
        mapping = {}

        # Get status options from project fields
        status_options = []
        fields = self.project_data.get("fields", {}).get("nodes", [])
        for field in fields:
            if field.get("name") == "Status":
                options = field.get("options", [])
                status_options = [opt["name"] for opt in options]
                break

        # If no status field found, fallback to hardcoded defaults
        if not status_options:
            return {
                "Todo": "TODO",
                "In Progress": "STRT",
                "Done": "DONE",
                "Someday": "WAIT"
            }

        # Heuristics for mapping
        used_keywords = set()

        for status in status_options:
            lower_status = status.lower()
            keyword = None

            # Common mappings
            if lower_status in ["todo", "to do", "open"]:
                keyword = "TODO"
            elif lower_status in ["in progress", "doing", "active"]:
                keyword = "STRT"
            elif lower_status in ["done", "closed", "complete", "completed"]:
                keyword = "DONE"
            elif lower_status in ["someday", "backlog", "icebox", "blocked", "waiting"]:
                keyword = "WAIT"

            # Fallback: Generate from name
            if not keyword or keyword in used_keywords:
                candidate = status.upper().replace(" ", "_")
                base_candidate = candidate
                counter = 1
                while candidate in used_keywords:
                    candidate = f"{base_candidate}_{counter}"
                    counter += 1
                keyword = candidate

            mapping[status] = keyword
            used_keywords.add(keyword)

        return mapping

    def _validate_status_map(self):
        """Ensure that the status mapping is 1:1 (injective)."""
        values = list(self.status_map.values())
        if len(values) != len(set(values)):
            seen = set()
            duplicates = set()
            for x in values:
                if x in seen:
                    duplicates.add(x)
                seen.add(x)
            raise ValueError(
                f"Status mapping must be 1:1. Duplicate Org keywords found: {duplicates}"
            )

    def _parse_status_map(self, map_str):
        mapping = {}
        try:
            parts = shlex.split(map_str)
            for part in parts:
                if '=' in part:
                    key, value = part.rsplit('=', 1)
                    mapping[key] = value
        except Exception:
            pass
        return mapping

    def _generate_status_map_str(self):
        parts = []
        for key, value in self.status_map.items():
            if " " in key:
                parts.append(f'"{key}"={value}')
            else:
                parts.append(f'{key}={value}')
        return " ".join(parts)

    def _get_priority_field_options(self):
        """Find and return priority field options from project data."""
        fields = self.project_data.get("fields", {}).get("nodes", [])
        for field in fields:
            if field.get("name", "").lower() == "priority":
                return field.get("options", [])
        return []

    def _detect_priority_scheme(self, options):
        """Detect if priority options are text-based or P-numbered."""
        option_names = [opt["name"].lower() for opt in options]

        # Check for P-numbered scheme (P0, P1, P2, etc.)
        p_pattern = re.compile(r'^p\d+$')
        if all(p_pattern.match(name) for name in option_names):
            return "p-numbered"

        # Check for text-based scheme (low, medium, high, etc.)
        text_keywords = {"low", "medium", "high", "critical", "urgent", "normal"}
        if any(name in text_keywords for name in option_names):
            return "text-based"

        return "custom"

    def _generate_default_priority_map(self):
        """Generate priority mapping based on detected scheme."""
        options = self._get_priority_field_options()
        if not options:
            return {}, None

        scheme = self._detect_priority_scheme(options)
        mapping = {}

        if scheme == "p-numbered":
            for opt in options:
                name = opt["name"]
                match = re.match(r'^[Pp](\d+)$', name)
                if match:
                    num = int(match.group(1))
                    if num <= 25:
                        mapping[name] = chr(ord('A') + num)

        elif scheme == "text-based":
            text_mappings = {
                "low": "C",
                "medium": "B",
                "high": "A",
                "critical": "A",
                "urgent": "A",
                "normal": "B",
            }
            for opt in options:
                name = opt["name"]
                lower_name = name.lower()
                if lower_name in text_mappings:
                    mapping[name] = text_mappings[lower_name]

        else:
            for i, opt in enumerate(options):
                if i <= 25:
                    mapping[opt["name"]] = chr(ord('A') + i)

        return mapping, scheme

    def _detect_priority_scheme_from_map(self):
        """Detect scheme from existing priority map."""
        if not self.priority_map:
            return None
        keys = list(self.priority_map.keys())
        p_pattern = re.compile(r'^[Pp]\d+$')
        if all(p_pattern.match(k) for k in keys):
            return "p-numbered"
        text_keywords = {"low", "medium", "high", "critical", "urgent", "normal"}
        if any(k.lower() in text_keywords for k in keys):
            return "text-based"
        return "custom"

    def _parse_priority_map(self, map_str):
        """Parse priority map string like 'Low=C Medium=B High=A'."""
        mapping = {}
        try:
            parts = shlex.split(map_str)
            for part in parts:
                if '=' in part:
                    key, value = part.rsplit('=', 1)
                    mapping[key] = value
        except Exception:
            pass
        return mapping

    def _generate_priority_map_str(self):
        """Generate priority map string for persistence."""
        if not self.priority_map:
            return None
        parts = []
        for key, value in self.priority_map.items():
            if " " in key:
                parts.append(f'"{key}"={value}')
            else:
                parts.append(f'{key}={value}')
        return " ".join(parts)

    def _get_priorities_header(self):
        """Generate #+PRIORITIES header based on scheme."""
        if not self.priority_map:
            return None

        values = list(self.priority_map.values())
        if not values:
            return None

        sorted_values = sorted(values)
        max_pri = sorted_values[0]
        min_pri = sorted_values[-1]

        if self.priority_scheme == "p-numbered":
            default_pri = "A"
        else:
            default_pri = "B" if "B" in values else max_pri

        return f"#+PRIORITIES: {max_pri} {min_pri} {default_pri}"

    def _extract_status_colors(self):
        """Extract status colors from project fields and map to org keywords."""
        colors = {}
        fields = self.project_data.get("fields", {}).get("nodes", [])
        for field in fields:
            if field.get("name") == "Status":
                for opt in field.get("options", []):
                    github_status = opt.get("name")
                    github_color = opt.get("color")
                    if github_status and github_color:
                        org_keyword = self.status_map.get(github_status)
                        if org_keyword:
                            colors[org_keyword] = github_color
                break
        return colors

    def _parse_status_colors(self, colors_str):
        """Parse status colors string like 'TODO=YELLOW STRT=PURPLE'."""
        colors = {}
        try:
            parts = shlex.split(colors_str)
            for part in parts:
                if '=' in part:
                    key, value = part.rsplit('=', 1)
                    colors[key] = value
        except Exception:
            pass
        return colors

    def _generate_status_colors_str(self):
        """Generate status colors string for persistence."""
        if not self.status_colors:
            return None
        parts = []
        for keyword, color in self.status_colors.items():
            parts.append(f'{keyword}={color}')
        return " ".join(parts)

    def convert(self):
        """Convert project data to Org-mode string."""
        lines = []

        # File header
        lines.append(f"#+TITLE: {self.project_data.get('title', 'GitHub Project')}")
        if self.project_url:
            lines.append(f"#+GITHUB_PROJECT_URL: {self.project_url}")

        if self.exclude_statuses:
            lines.append(
                f"#+GITHUB_EXCLUDE_STATUSES: {' '.join(self.exclude_statuses)}"
            )

        lines.append(f"#+GITHUB_STATUS_MAP: {self.status_map_str}")

        if self.priority_map_str:
            lines.append(f"#+GITHUB_PRIORITY_MAP: {self.priority_map_str}")

        if self.status_colors_str:
            lines.append(f"#+GITHUB_STATUS_COLORS: {self.status_colors_str}")

        lines.append(f"#+DATE: {datetime.date.today()}")

        priorities_header = self._get_priorities_header()
        if priorities_header:
            lines.append(priorities_header)

        # Generate #+TODO line dynamically
        todo_keywords = []
        done_keywords = []

        for status, keyword in self.status_map.items():
            if status.lower() in ["done", "closed", "complete", "completed"]:
                done_keywords.append(keyword)
            else:
                todo_keywords.append(keyword)

        if not done_keywords and "DONE" in self.status_map.values():
            done_keywords.append("DONE")
            if "DONE" in todo_keywords:
                todo_keywords.remove("DONE")

        todo_str = " ".join(todo_keywords)
        done_str = " ".join(done_keywords)

        if done_str:
            lines.append(f"#+TODO: {todo_str} | {done_str}")
        else:
            lines.append(f"#+TODO: {todo_str}")

        lines.append("#+STARTUP: show2levels")
        lines.append("")

        items = self.project_data.get("items", {}).get("nodes", [])
        for item in items:
            lines.extend(self._convert_item(item))

        if self.add_local_variables:
            lines.append("")
            lines.append("* COMMENT Local Variables")
            lines.append("# Local Variables:")
            lines.append("# eval: (project-to-org-mode 1)")
            lines.append("# End:")

        return "\n".join(lines)

    def _convert_item(self, item):
        """Convert a single project item to Org lines."""
        content = item.get("content", {})
        if not content:
            return []

        title = content.get("title", "No Title")
        body = content.get("body", "")
        url = content.get("url", "")
        item_type = item.get("type")

        fields = self._parse_field_values(
            item.get("fieldValues", {}).get("nodes", [])
        )

        status = fields.get("Status")
        if status and status in self.status_map:
            todo_keyword = self.status_map[status]
        else:
            todo_keyword = (
                next(iter(self.status_map.values()), "TODO")
                if self.status_map
                else "TODO"
            )

        if status in self.exclude_statuses or todo_keyword in self.exclude_statuses:
            return []

        priority_cookie = ""
        priority_value = fields.get("Priority")
        if priority_value and self.priority_map:
            org_priority = self.priority_map.get(priority_value)
            if org_priority:
                priority_cookie = f" [#{org_priority}]"

        lines = []
        lines.append(f"* {todo_keyword}{priority_cookie} {title}")

        lines.append(":PROPERTIES:")
        lines.append(f":ID: {item.get('id')}")
        if url:
            lines.append(f":URL: {url}")
        if item_type == "ISSUE":
            lines.append(f":ISSUE_NUMBER: {content.get('number')}")

        for name, value in fields.items():
            if name != "Status" and name != "Title":
                safe_key = name.upper().replace(" ", "_")
                lines.append(f":{safe_key}: {value}")

        assignees = [
            node["login"]
            for node in content.get("assignees", {}).get("nodes", [])
        ]
        if assignees:
            lines.append(f":ASSIGNEES: {', '.join(assignees)}")

        labels = [
            node["name"]
            for node in content.get("labels", {}).get("nodes", [])
        ]
        if labels:
            lines.append(f":LABELS: {', '.join(labels)}")

        lines.append(":END:")

        if body:
            lines.append("")
            lines.append(self._process_body(body))
            lines.append("")

        return lines

    def _process_body(self, body):
        """Convert GitHub Markdown body to Org-mode syntax."""
        text = body

        # Normalize line endings
        text = text.replace('\r\n', '\n').replace('\r', '\n')

        # Convert markdown lists to org lists (must be before bold/italic)
        # Handles: * item, + item (- item already works in org)
        text = re.sub(r'^(\s*)\* ', r'\1- ', text, flags=re.MULTILINE)
        text = re.sub(r'^(\s*)\+ ', r'\1- ', text, flags=re.MULTILINE)

        # Code blocks
        text = re.sub(
            r'```(\w*)\n(.*?)```',
            lambda m: f'#+BEGIN_SRC {m.group(1)}\n{m.group(2)}#+END_SRC',
            text,
            flags=re.DOTALL
        )

        # Inline code
        text = re.sub(r'`([^`\n]+)`', r'=\1=', text)

        # Images
        text = re.sub(r'!\[([^\]]*)\]\(([^)]+)\)', r'[[\2]]', text)

        # Links
        text = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'[[\2][\1]]', text)

        # Bold (use placeholders)
        text = re.sub(r'\*\*([^*]+)\*\*', r'⟦BOLD⟧\1⟦/BOLD⟧', text)
        text = re.sub(r'__([^_]+)__', r'⟦BOLD⟧\1⟦/BOLD⟧', text)

        # Italic
        text = re.sub(r'(?<![*⟧])\*([^*]+)\*(?![*⟦])', r'/\1/', text)
        text = re.sub(r'(?<![a-zA-Z0-9])_([^_]+)_(?![a-zA-Z0-9])', r'/\1/', text)

        # Replace bold placeholders
        text = text.replace('⟦BOLD⟧', '*').replace('⟦/BOLD⟧', '*')

        # Strikethrough
        text = re.sub(r'~~([^~]+)~~', r'+\1+', text)

        # Blockquotes
        text = re.sub(r'^>\s*(.*)$', r': \1', text, flags=re.MULTILINE)

        # Checkboxes
        text = re.sub(r'^(\s*[-+*]\s+)\[x\]', r'\1[X]', text, flags=re.MULTILINE)

        # Horizontal rules
        text = re.sub(r'^(---+|\*\*\*+|___+)\s*$', r'-----', text, flags=re.MULTILINE)

        # GitHub @mentions
        text = re.sub(
            r'(?<![a-zA-Z0-9])@([a-zA-Z0-9][-a-zA-Z0-9]*)',
            r'[[https://github.com/\1][@\1]]',
            text
        )

        # Trim trailing whitespace
        text = '\n'.join(line.rstrip() for line in text.split('\n'))

        return text

    def _parse_field_values(self, nodes):
        """Extract field names and values from the nested GraphQL structure."""
        fields = {}
        for node in nodes:
            if not node:
                continue

            field_info = node.get("field", {})
            field_name = field_info.get("name")

            if not field_name:
                continue

            value = None
            if "name" in node and "field" in node:
                value = node["name"]
            elif "text" in node:
                value = node["text"]
            elif "date" in node:
                value = node["date"]
            elif "title" in node:
                value = node["title"]

            if value is not None:
                fields[field_name] = value

        return fields


# =============================================================================
# Main
# =============================================================================

def main():
    parser = argparse.ArgumentParser(description="Sync GitHub Projects to Org-mode")
    parser.add_argument(
        "--org-file",
        help="Path to the Org file to sync",
        required=False
    )
    parser.add_argument(
        "--project-url",
        help="GitHub Project URL",
        required=True
    )
    parser.add_argument(
        "--exclude-statuses",
        help="List of statuses to exclude",
        nargs="*",
        default=[]
    )
    parser.add_argument(
        "--status-map",
        help="Status mapping string (e.g. 'Todo=TODO \"In Progress\"=STRT')",
        required=False
    )
    parser.add_argument(
        "--priority-map",
        help="Priority mapping string (e.g. 'Low=C Medium=B High=A')",
        required=False
    )
    parser.add_argument(
        "--no-local-variables",
        help="Don't add Local Variables block to enable project-to-org-mode",
        action="store_true",
        default=False
    )

    args = parser.parse_args()

    try:
        token = get_github_token()
        owner_type, owner, number = parse_project_url(args.project_url)

        project_data = fetch_project_items(owner_type, owner, number, token)

        # Extract existing config if file exists
        existing_config = {}
        if args.org_file:
            existing_config = extract_config_from_org(args.org_file)

        # Determine mappings: CLI > File > Default
        status_map_to_use = args.status_map or existing_config.get('status_map')
        priority_map_to_use = args.priority_map or existing_config.get('priority_map')
        status_colors_to_use = existing_config.get('status_colors')

        exclude_statuses_to_use = args.exclude_statuses
        if not exclude_statuses_to_use and existing_config.get('exclude_statuses'):
            exclude_statuses_to_use = existing_config.get('exclude_statuses')

        converter = OrgConverter(
            project_data,
            project_url=args.project_url,
            exclude_statuses=exclude_statuses_to_use,
            status_map_str=status_map_to_use,
            priority_map_str=priority_map_to_use,
            status_colors_str=status_colors_to_use,
            add_local_variables=not args.no_local_variables
        )
        org_content = converter.convert()

        if args.org_file:
            with open(args.org_file, "w") as f:
                f.write(org_content)
            print(f"Successfully synced to {args.org_file}")
        else:
            print(org_content)

    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


if __name__ == "__main__":
    main()
