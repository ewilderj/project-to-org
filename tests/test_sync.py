import os
import pytest
import requests
from project_to_org.github_client import get_github_token, parse_project_url, fetch_project_items
from project_to_org.org_converter import OrgConverter

# Constants for the test project
TEST_PROJECT_URL = "https://github.com/users/ewilderj/projects/2"
TEST_PROJECT_NUMBER = 2
TEST_OWNER = "ewilderj"
TEST_OWNER_TYPE = "user"

@pytest.fixture
def github_token():
    return get_github_token()

def test_fetch_empty_project(github_token):
    """Test fetching a project that we expect to exist."""
    # Note: We can't easily programmatically create items via GraphQL in this test 
    # without more complex setup (finding column IDs etc). 
    # So for now we just verify we can fetch the project itself.
    
    project_data = fetch_project_items(TEST_OWNER_TYPE, TEST_OWNER, TEST_PROJECT_NUMBER, github_token)
    
    assert project_data is not None
    assert "title" in project_data
    assert "items" in project_data

def test_org_conversion_structure():
    """Test that the Org converter produces expected structure from mock data."""
    mock_data = {
        "title": "Test Project",
        "items": {
            "nodes": [
                {
                    "id": "123",
                    "type": "ISSUE",
                    "content": {
                        "title": "Test Issue",
                        "body": "This is a test body.",
                        "number": 1,
                        "url": "http://github.com/issue/1",
                        "assignees": {"nodes": [{"login": "user1"}]},
                        "labels": {"nodes": [{"name": "bug"}]}
                    },
                    "fieldValues": {
                        "nodes": [
                            {"name": "Todo", "field": {"name": "Status"}},
                            {"name": "High", "field": {"name": "Priority"}}
                        ]
                    }
                }
            ]
        }
    }
    
    converter = OrgConverter(mock_data, project_url="https://github.com/users/ewilderj/projects/2")
    org_output = converter.convert()
    
    assert "#+TITLE: Test Project" in org_output
    assert "#+GITHUB_PROJECT_URL: https://github.com/users/ewilderj/projects/2" in org_output
    assert "* TODO Test Issue" in org_output
    assert ":PROPERTIES:" in org_output
    assert ":ID: 123" in org_output
    assert ":PRIORITY: High" in org_output
    assert ":ASSIGNEES: user1" in org_output
    assert ":LABELS: bug" in org_output
    assert "This is a test body." in org_output

def test_status_mapping():
    """Test that status mapping works correctly."""
    mock_data = {
        "title": "Test Project",
        "items": {
            "nodes": [
                {
                    "id": "1",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "Draft 1"},
                    "fieldValues": {
                        "nodes": [{"name": "In Progress", "field": {"name": "Status"}}]
                    }
                },
                {
                    "id": "2",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "Draft 2"},
                    "fieldValues": {
                        "nodes": [{"name": "Done", "field": {"name": "Status"}}]
                    }
                }
            ]
        }
    }
    
    converter = OrgConverter(mock_data, project_url="https://github.com/users/ewilderj/projects/2")
    org_output = converter.convert()
    
    assert "* STRT Draft 1" in org_output
    assert "* DONE Draft 2" in org_output

def test_exclude_statuses():
    """Test that items with excluded statuses are filtered out."""
    mock_data = {
        "title": "Test Project",
        "items": {
            "nodes": [
                {
                    "id": "1",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "Keep Me"},
                    "fieldValues": {
                        "nodes": [{"name": "Todo", "field": {"name": "Status"}}]
                    }
                },
                {
                    "id": "2",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "Exclude Me"},
                    "fieldValues": {
                        "nodes": [{"name": "Done", "field": {"name": "Status"}}]
                    }
                },
                {
                    "id": "3",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "Exclude Me Too"},
                    "fieldValues": {
                        "nodes": [{"name": "Someday", "field": {"name": "Status"}}]
                    }
                }
            ]
        }
    }
    
    converter = OrgConverter(mock_data, project_url="https://github.com/users/ewilderj/projects/2", exclude_statuses=["Done", "Someday"])
    org_output = converter.convert()
    
    assert "* TODO Keep Me" in org_output
    assert "Exclude Me" not in org_output
    assert "Exclude Me Too" not in org_output
    assert "#+GITHUB_EXCLUDE_STATUSES: Done Someday" in org_output

def test_exclude_statuses_persistence():
    """Test that the exclude statuses header is persisted correctly."""
    mock_data = {
        "title": "Test Project",
        "items": {"nodes": []}
    }
    
    # Case 1: statuses provided -> header present
    converter = OrgConverter(mock_data, exclude_statuses=["Done", "Someday"])
    output = converter.convert()
    assert "#+GITHUB_EXCLUDE_STATUSES: Done Someday" in output
    
    # Case 2: no statuses provided -> header absent
    converter = OrgConverter(mock_data, exclude_statuses=[])
    output = converter.convert()
    assert "#+GITHUB_EXCLUDE_STATUSES" not in output

def test_custom_status_map():
    """Test that custom status mapping works."""
    mock_data = {
        "title": "Test Project",
        "items": {
            "nodes": [
                {
                    "id": "1",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "Custom Status"},
                    "fieldValues": {
                        "nodes": [{"name": "My Custom Status", "field": {"name": "Status"}}]
                    }
                }
            ]
        }
    }
    
    # Map "My Custom Status" to "CUSTOM"
    map_str = '"My Custom Status"=CUSTOM'
    converter = OrgConverter(mock_data, status_map_str=map_str)
    org_output = converter.convert()
    
    assert "* CUSTOM Custom Status" in org_output
    assert f"#+GITHUB_STATUS_MAP: {map_str}" in org_output

def test_status_mapping_uniqueness():
    """Test that non-unique status mapping raises an error."""
    mock_data = {"title": "Test", "items": {"nodes": []}}
    
    # Duplicate mapping: Todo -> TODO, Backlog -> TODO
    map_str = 'Todo=TODO Backlog=TODO'
    
    with pytest.raises(ValueError, match="Status mapping must be 1:1"):
        OrgConverter(mock_data, status_map_str=map_str)

def test_generate_default_status_map_heuristics():
    # Mock project data with field options
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [
                        {"name": "Todo"},
                        {"name": "In Progress"},
                        {"name": "Done"},
                        {"name": "Blocked"},
                        {"name": "Needs Review"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data, status_map_str=None)
    
    expected_map = {
        "Todo": "TODO",
        "In Progress": "STRT",
        "Done": "DONE",
        "Blocked": "WAIT", # Heuristic maps Blocked to WAIT
        "Needs Review": "NEEDS_REVIEW"
    }
    
    assert converter.status_map == expected_map

def test_generate_default_status_map_collision_handling():
    # Test that we handle collisions by generating unique keywords
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [
                        {"name": "My Status"},
                        {"name": "my status"} 
                    ]
                }
            ]
        }
    }
    
    # "My Status" -> "MY_STATUS"
    # "my status" -> "MY_STATUS" -> Collision! -> Should become "MY_STATUS_1" or similar
    
    converter = OrgConverter(project_data, status_map_str=None)
    
    # Check that we have two entries and values are unique
    assert len(converter.status_map) == 2
    values = list(converter.status_map.values())
    assert len(set(values)) == 2
    assert "MY_STATUS" in values
    assert "MY_STATUS_1" in values

def test_todo_line_generation():
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [
                        {"name": "Todo"},
                        {"name": "In Progress"},
                        {"name": "Done"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data, status_map_str=None)
    output = converter.convert()
    
    assert "#+TODO: TODO STRT | DONE" in output
    # Check for space-separated map with quotes for keys with spaces
    assert '#+GITHUB_STATUS_MAP: Todo=TODO "In Progress"=STRT Done=DONE' in output

def test_todo_line_generation_custom_statuses():
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [
                        {"name": "Backlog"},
                        {"name": "Coding"},
                        {"name": "Shipped"} # Should be treated as done?
                    ]
                }
            ]
        }
    }
    
    # "Shipped" isn't in our hardcoded "done" list in the test above (unless I check the implementation).
    # Implementation has: ["done", "closed", "complete", "completed"]
    # So "Shipped" will be a TODO state.
    # "Backlog" maps to "WAIT" by heuristic.
    
    converter = OrgConverter(project_data, status_map_str=None)
    output = converter.convert()
    
    # Backlog -> WAIT
    # Coding -> CODING
    # Shipped -> SHIPPED
    
    assert "#+TODO: WAIT CODING SHIPPED" in output
    # No pipe because no recognized done state
