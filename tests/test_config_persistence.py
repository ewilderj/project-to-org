import os
import tempfile
from project_to_org.org_converter import extract_config_from_org

def test_extract_config_from_org():
    content = """
#+TITLE: Test Project
#+GITHUB_STATUS_MAP: Todo=TODO "In Progress"=WIP Done=DONE
#+GITHUB_EXCLUDE_STATUSES: Archived
#+DATE: 2026-01-01
* TODO Item 1
    """
    
    with tempfile.NamedTemporaryFile(mode='w', delete=False) as tmp:
        tmp.write(content)
        tmp_path = tmp.name
        
    try:
        config = extract_config_from_org(tmp_path)
        
        assert config['status_map'] == 'Todo=TODO "In Progress"=WIP Done=DONE'
        assert config['exclude_statuses'] == ['Archived']
        
    finally:
        os.remove(tmp_path)

def test_extract_config_missing_file():
    config = extract_config_from_org("nonexistent.org")
    assert config == {}
