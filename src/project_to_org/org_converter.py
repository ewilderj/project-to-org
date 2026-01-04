import datetime
import shlex
import re
import os

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
    def __init__(self, project_data, project_url=None, exclude_statuses=None, status_map_str=None, priority_map_str=None, status_colors_str=None):
        self.project_data = project_data
        self.project_url = project_url
        self.exclude_statuses = exclude_statuses or []
        
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
            self.priority_map_str = self._generate_priority_map_str() if self.priority_map else None

        # Status colors (GitHub status -> org keyword -> color)
        if status_colors_str:
            self.status_colors = self._parse_status_colors(status_colors_str)
            self.status_colors_str = status_colors_str
        else:
            self.status_colors = self._extract_status_colors()
            self.status_colors_str = self._generate_status_colors_str() if self.status_colors else None

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
        
        # If no status field found, fallback to hardcoded defaults or empty
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
            
            # 1. Common mappings
            if lower_status in ["todo", "to do", "open"]:
                keyword = "TODO"
            elif lower_status in ["in progress", "doing", "active"]:
                keyword = "STRT"
            elif lower_status in ["done", "closed", "complete", "completed"]:
                keyword = "DONE"
            elif lower_status in ["someday", "backlog", "icebox", "blocked", "waiting"]:
                keyword = "WAIT"
            
            # 2. Fallback: Generate from name
            if not keyword or keyword in used_keywords:
                # Generate a keyword: UPPERCASE, replace spaces with _, max 4 chars if possible?
                # Actually Org keywords can be longer. Let's just use UPPERCASE_UNDERSCORE.
                candidate = status.upper().replace(" ", "_")
                
                # Ensure uniqueness
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
            # Find duplicates for better error message
            seen = set()
            duplicates = set()
            for x in values:
                if x in seen:
                    duplicates.add(x)
                seen.add(x)
            raise ValueError(f"Status mapping must be 1:1. Duplicate Org keywords found: {duplicates}")

    def _parse_status_map(self, map_str):
        mapping = {}
        try:
            parts = shlex.split(map_str)
            for part in parts:
                if '=' in part:
                    key, value = part.rsplit('=', 1)
                    mapping[key] = value
        except Exception:
            # Fallback or log error? For now, return empty or default?
            # Let's just return what we parsed so far or empty
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
        
        # Unknown scheme - will need explicit mapping
        return "custom"

    def _generate_default_priority_map(self):
        """Generate priority mapping based on detected scheme."""
        options = self._get_priority_field_options()
        if not options:
            return {}, None
        
        scheme = self._detect_priority_scheme(options)
        mapping = {}
        
        if scheme == "p-numbered":
            # P0=A, P1=B, P2=C, P3=D, etc.
            for opt in options:
                name = opt["name"]
                match = re.match(r'^[Pp](\d+)$', name)
                if match:
                    num = int(match.group(1))
                    # A=0, B=1, C=2, D=3, etc.
                    if num <= 25:  # A-Z
                        mapping[name] = chr(ord('A') + num)
                        
        elif scheme == "text-based":
            # Map common text priorities
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
            # Custom scheme - generate sequential letters
            for i, opt in enumerate(options):
                if i <= 25:  # A-Z
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
            
        # Sort to find min and max
        sorted_values = sorted(values)
        max_pri = sorted_values[0]   # A is highest (lowest letter)
        min_pri = sorted_values[-1]  # C/D is lowest (highest letter)
        
        # Default priority
        if self.priority_scheme == "p-numbered":
            # Managers think everything is P0, so default to A
            default_pri = "A"
        else:
            # For text-based, default to middle (B)
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
                        # Map to org keyword
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
        
        # Persist the exclude_statuses setting
        if self.exclude_statuses:
            lines.append(f"#+GITHUB_EXCLUDE_STATUSES: {' '.join(self.exclude_statuses)}")
            
        # Persist the status map
        lines.append(f"#+GITHUB_STATUS_MAP: {self.status_map_str}")
        
        # Persist the priority map if we have one
        if self.priority_map_str:
            lines.append(f"#+GITHUB_PRIORITY_MAP: {self.priority_map_str}")
        
        # Persist the status colors if we have them
        if self.status_colors_str:
            lines.append(f"#+GITHUB_STATUS_COLORS: {self.status_colors_str}")
        
        lines.append(f"#+DATE: {datetime.date.today()}")
        
        # Generate #+PRIORITIES line if we have priority mapping
        priorities_header = self._get_priorities_header()
        if priorities_header:
            lines.append(priorities_header)
        
        # Generate #+TODO line dynamically
        todo_keywords = []
        done_keywords = []
        
        for status, keyword in self.status_map.items():
            # Heuristic: If status looks like "Done", put it in done keywords
            if status.lower() in ["done", "closed", "complete", "completed"]:
                done_keywords.append(keyword)
            else:
                todo_keywords.append(keyword)
        
        # If no done keywords found, but we have keywords, assume the last one is done? 
        # Or just put everything in TODO if we can't tell.
        # Let's default to "DONE" if it exists in values, otherwise just list them.
        
        if not done_keywords and "DONE" in self.status_map.values():
             done_keywords.append("DONE")
             if "DONE" in todo_keywords: todo_keywords.remove("DONE")

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
            
        return "\n".join(lines)

    def _convert_item(self, item):
        """Convert a single project item to Org lines."""
        content = item.get("content", {})
        if not content:
            return []
            
        # Extract basic info
        title = content.get("title", "No Title")
        body = content.get("body", "")
        url = content.get("url", "")
        item_type = item.get("type")
        
        # Parse field values
        fields = self._parse_field_values(item.get("fieldValues", {}).get("nodes", []))
        
        # Determine TODO keyword from Status field
        # Use first keyword in status_map as default for items without status
        status = fields.get("Status")
        if status and status in self.status_map:
            todo_keyword = self.status_map[status]
        else:
            # Default to first keyword in the map (or "TODO" if map is empty)
            todo_keyword = next(iter(self.status_map.values()), "TODO") if self.status_map else "TODO"
        
        # Filter out excluded statuses (accept either GitHub status name or Org keyword)
        if status in self.exclude_statuses or todo_keyword in self.exclude_statuses:
            return []
        
        # Determine priority cookie if Priority field exists
        priority_cookie = ""
        priority_value = fields.get("Priority")
        if priority_value and self.priority_map:
            org_priority = self.priority_map.get(priority_value)
            if org_priority:
                priority_cookie = f" [#{org_priority}]"
        
        # Build heading
        lines = []
        lines.append(f"* {todo_keyword}{priority_cookie} {title}")
        
        # Properties drawer
        lines.append(":PROPERTIES:")
        lines.append(f":ID: {item.get('id')}")
        if url:
            lines.append(f":URL: {url}")
        if item_type == "ISSUE":
            lines.append(f":ISSUE_NUMBER: {content.get('number')}")
            
        # Add all other fields to properties
        for name, value in fields.items():
            if name != "Status" and name != "Title": # Title is already in heading
                # Org properties keys should be uppercase and usually no spaces (though spaces are allowed in some contexts, better to sanitize)
                safe_key = name.upper().replace(" ", "_")
                lines.append(f":{safe_key}: {value}")
                
        # Add assignees and labels if present
        assignees = [node["login"] for node in content.get("assignees", {}).get("nodes", [])]
        if assignees:
            lines.append(f":ASSIGNEES: {', '.join(assignees)}")
            
        labels = [node["name"] for node in content.get("labels", {}).get("nodes", [])]
        # We could also put labels in Org tags, but let's put them in properties for now as requested
        if labels:
            lines.append(f":LABELS: {', '.join(labels)}")
            
        lines.append(":END:")
        
        # Body content
        if body:
            lines.append("")
            lines.append(self._process_body(body))
            lines.append("")
            
        return lines

    def _process_body(self, body):
        """Convert GitHub Markdown body to Org-mode syntax."""
        text = body
        
        # 0. Normalize line endings (remove Windows CR characters)
        text = text.replace('\r\n', '\n').replace('\r', '\n')
        
        # 1. Code blocks (do first to protect content inside)
        # ```language\ncode\n``` → #+BEGIN_SRC language\ncode\n#+END_SRC
        text = re.sub(
            r'```(\w*)\n(.*?)```',
            lambda m: f'#+BEGIN_SRC {m.group(1)}\n{m.group(2)}#+END_SRC',
            text,
            flags=re.DOTALL
        )
        
        # 2. Inline code: `code` → =code=
        # Be careful not to match inside code blocks (already converted)
        text = re.sub(r'`([^`\n]+)`', r'=\1=', text)
        
        # 3. Images: ![alt](url) → [[url]]
        text = re.sub(r'!\[([^\]]*)\]\(([^)]+)\)', r'[[\2]]', text)
        
        # 4. Links: [text](url) → [[url][text]]
        text = re.sub(r'\[([^\]]+)\]\(([^)]+)\)', r'[[\2][\1]]', text)
        
        # 5. Bold: **text** or __text__ → *text*
        # Use a placeholder to avoid italic regex matching these
        text = re.sub(r'\*\*([^*]+)\*\*', r'⟦BOLD⟧\1⟦/BOLD⟧', text)
        text = re.sub(r'__([^_]+)__', r'⟦BOLD⟧\1⟦/BOLD⟧', text)
        
        # 6. Italic: *text* or _text_ → /text/
        # Must come after bold conversion
        # Be careful with underscores in words (snake_case)
        text = re.sub(r'(?<![*⟧])\*([^*]+)\*(?![*⟦])', r'/\1/', text)
        text = re.sub(r'(?<![a-zA-Z0-9])_([^_]+)_(?![a-zA-Z0-9])', r'/\1/', text)
        
        # 7. Replace bold placeholders with Org bold
        text = text.replace('⟦BOLD⟧', '*').replace('⟦/BOLD⟧', '*')
        
        # 8. Strikethrough: ~~text~~ → +text+
        text = re.sub(r'~~([^~]+)~~', r'+\1+', text)
        
        # 9. Blockquotes: > text → : text (Org fixed-width/quote style)
        text = re.sub(r'^>\s*(.*)$', r': \1', text, flags=re.MULTILINE)
        
        # 10. Checkboxes: - [x] → - [X] (uppercase for Org)
        text = re.sub(r'^(\s*[-+*]\s+)\[x\]', r'\1[X]', text, flags=re.MULTILINE)
        
        # 11. Horizontal rules: --- or *** or ___ → -----
        text = re.sub(r'^(---+|\*\*\*+|___+)\s*$', r'-----', text, flags=re.MULTILINE)
        
        # 12. GitHub @mentions: @username → [[https://github.com/username][@username]]
        text = re.sub(r'(?<![a-zA-Z0-9])@([a-zA-Z0-9][-a-zA-Z0-9]*)', 
                      r'[[https://github.com/\1][@\1]]', text)
        
        # 13. Headers: # H1, ## H2 → not converted (would conflict with org headings)
        # Leave as-is or could prefix with "* " but that creates sub-headings
        
        # 14. Trim trailing whitespace from each line
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
                
            # Extract value based on type (the structure varies)
            value = None
            if "name" in node and "field" in node: # Single Select
                value = node["name"]
            elif "text" in node: # Text
                value = node["text"]
            elif "date" in node: # Date
                value = node["date"]
            elif "title" in node: # Iteration
                value = node["title"]
                
            if value is not None:
                fields[field_name] = value
                
        return fields
