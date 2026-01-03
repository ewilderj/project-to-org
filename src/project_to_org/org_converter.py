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
    return config

class OrgConverter:
    def __init__(self, project_data, project_url=None, exclude_statuses=None, status_map_str=None):
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
        
        lines.append(f"#+DATE: {datetime.date.today()}")
        
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
        status = fields.get("Status", "Todo")
        todo_keyword = self.status_map.get(status, "TODO")
        
        # Filter out excluded statuses
        if status in self.exclude_statuses:
            return []
        
        # Build heading
        lines = []
        lines.append(f"* {todo_keyword} {title}")
        
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
        """Process the body content to be Org-mode compatible."""
        # Convert GitHub checkboxes [x] to Org checkboxes [X]
        # Matches - [x], * [x], + [x] with optional indentation
        return re.sub(r"^(\s*[-+*]\s+)\[x\]", r"\1[X]", body, flags=re.MULTILINE)

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
