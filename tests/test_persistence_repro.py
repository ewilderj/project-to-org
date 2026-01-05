import os
import tempfile
from unittest.mock import patch
from project_to_org import main
import pytest

# Mock data
MOCK_PROJECT_DATA = {
    "title": "Test Project",
    "fields": {
        "nodes": [
            {
                "name": "Status",
                "options": [
                    {"name": "Todo"},
                    {"name": "Done"}
                ]
            }
        ]
    },
    "items": {"nodes": []}
}

def test_persistence_repro():
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as tmp:
        org_file_path = tmp.name

    try:
        # 1. First Sync
        with patch('project_to_org.fetch_project_items', return_value=MOCK_PROJECT_DATA), \
             patch('project_to_org.get_github_token', return_value="fake_token"), \
             patch('project_to_org.parse_project_url', return_value=("user", "owner", 1)), \
             patch('sys.argv', ["main.py", "--project-url", "https://github.com/users/owner/projects/1", "--org-file", org_file_path]):
            
            main()
            
        # Verify default map
        with open(org_file_path, 'r') as f:
            content = f.read()
        assert "#+GITHUB_STATUS_MAP: Todo=TODO Done=DONE" in content
        
        # 2. Modify the map in the file
        # We change TODO to MUSTDO
        new_map_line = "#+GITHUB_STATUS_MAP: Todo=MUSTDO Done=DONE"
        content = content.replace("#+GITHUB_STATUS_MAP: Todo=TODO Done=DONE", new_map_line)
        with open(org_file_path, 'w') as f:
            f.write(content)
            
        # 3. Second Sync
        with patch('project_to_org.fetch_project_items', return_value=MOCK_PROJECT_DATA), \
             patch('project_to_org.get_github_token', return_value="fake_token"), \
             patch('project_to_org.parse_project_url', return_value=("user", "owner", 1)), \
             patch('sys.argv', ["main.py", "--project-url", "https://github.com/users/owner/projects/1", "--org-file", org_file_path]):
            
            main()
            
        # 4. Verify the map is preserved
        with open(org_file_path, 'r') as f:
            new_content = f.read()
            
        print(f"DEBUG: Content after second sync:\n{new_content}")
        
        assert "#+GITHUB_STATUS_MAP: Todo=MUSTDO Done=DONE" in new_content
        
    finally:
        if os.path.exists(org_file_path):
            os.remove(org_file_path)
