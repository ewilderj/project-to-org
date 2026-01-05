import os
import pytest
import requests
from project_to_org import get_github_token, parse_project_url, fetch_project_items, OrgConverter, extract_config_from_org

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


def test_exclude_statuses_by_org_keyword():
    """Test that items can be excluded by Org keyword (not just GitHub status name)."""
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
                }
            ]
        },
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
        }
    }
    
    # Exclude by Org keyword "DONE" instead of GitHub status "Done"
    converter = OrgConverter(mock_data, exclude_statuses=["DONE"])
    org_output = converter.convert()
    
    assert "* TODO Keep Me" in org_output
    assert "Exclude Me" not in org_output


def test_missing_status_uses_first_keyword():
    """Test that items without a status use the first keyword in the map."""
    mock_data = {
        "title": "Test Project",
        "items": {
            "nodes": [
                {
                    "id": "1",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "No Status Item"},
                    "fieldValues": {
                        "nodes": []  # No status field
                    }
                },
                {
                    "id": "2",
                    "type": "DRAFT_ISSUE",
                    "content": {"title": "Unknown Status Item"},
                    "fieldValues": {
                        "nodes": [{"name": "SomeUnknownStatus", "field": {"name": "Status"}}]
                    }
                }
            ]
        },
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [
                        {"name": "Backlog"},
                        {"name": "Ready"},
                        {"name": "Done"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(mock_data)
    org_output = converter.convert()
    
    # Both items should use the first keyword (WAIT from Backlog) not hardcoded TODO
    # The auto-mapping generates Backlog=WAIT, Ready=READY, Done=DONE
    assert "* WAIT No Status Item" in org_output
    assert "* WAIT Unknown Status Item" in org_output
    assert "* TODO" not in org_output


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


# ============================================================================
# Priority Mapping Tests
# ============================================================================

def test_priority_text_based_detection():
    """Test that Low/Medium/High priority scheme is detected and mapped."""
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Priority",
                    "options": [
                        {"name": "Low"},
                        {"name": "Medium"},
                        {"name": "High"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data)
    
    assert converter.priority_scheme == "text-based"
    assert converter.priority_map == {"Low": "C", "Medium": "B", "High": "A"}
    
    output = converter.convert()
    assert "#+PRIORITIES: A C B" in output
    assert "#+GITHUB_PRIORITY_MAP: Low=C Medium=B High=A" in output


def test_priority_p_numbered_detection():
    """Test that P0/P1/P2 priority scheme is detected and mapped."""
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Priority",
                    "options": [
                        {"name": "P0"},
                        {"name": "P1"},
                        {"name": "P2"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data)
    
    assert converter.priority_scheme == "p-numbered"
    assert converter.priority_map == {"P0": "A", "P1": "B", "P2": "C"}
    
    output = converter.convert()
    # P-numbered defaults to A (managers think everything is P0)
    assert "#+PRIORITIES: A C A" in output
    assert "#+GITHUB_PRIORITY_MAP: P0=A P1=B P2=C" in output


def test_priority_p_numbered_extended():
    """Test P0-P3 scheme extends to D."""
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Priority",
                    "options": [
                        {"name": "P0"},
                        {"name": "P1"},
                        {"name": "P2"},
                        {"name": "P3"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data)
    
    assert converter.priority_map == {"P0": "A", "P1": "B", "P2": "C", "P3": "D"}
    
    output = converter.convert()
    assert "#+PRIORITIES: A D A" in output


def test_priority_in_heading():
    """Test that priority cookie is added to headings."""
    project_data = {
        "items": {
            "nodes": [
                {
                    "id": "item1",
                    "type": "ISSUE",
                    "content": {
                        "title": "High priority task",
                        "body": "",
                        "number": 1,
                        "url": "http://example.com/1",
                        "assignees": {"nodes": []},
                        "labels": {"nodes": []}
                    },
                    "fieldValues": {
                        "nodes": [
                            {"name": "Todo", "field": {"name": "Status"}},
                            {"name": "High", "field": {"name": "Priority"}}
                        ]
                    }
                },
                {
                    "id": "item2",
                    "type": "ISSUE",
                    "content": {
                        "title": "Low priority task",
                        "body": "",
                        "number": 2,
                        "url": "http://example.com/2",
                        "assignees": {"nodes": []},
                        "labels": {"nodes": []}
                    },
                    "fieldValues": {
                        "nodes": [
                            {"name": "Todo", "field": {"name": "Status"}},
                            {"name": "Low", "field": {"name": "Priority"}}
                        ]
                    }
                }
            ]
        },
        "fields": {
            "nodes": [
                {
                    "name": "Priority",
                    "options": [
                        {"name": "Low"},
                        {"name": "Medium"},
                        {"name": "High"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "* TODO [#A] High priority task" in output
    assert "* TODO [#C] Low priority task" in output


def test_priority_persistence():
    """Test that priority map is persisted and read from file."""
    import tempfile
    
    org_content = """#+TITLE: Test
#+GITHUB_PRIORITY_MAP: Critical=A Normal=B Deferred=C
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.org', delete=False) as f:
        f.write(org_content)
        f.flush()
        
        config = extract_config_from_org(f.name)
        assert config.get('priority_map') == "Critical=A Normal=B Deferred=C"
        
        # Clean up
        import os
        os.unlink(f.name)


def test_priority_custom_map_from_string():
    """Test that custom priority map is parsed from string."""
    project_data = {
        "items": {"nodes": []},
        "fields": {"nodes": []}
    }
    
    converter = OrgConverter(project_data, priority_map_str="Critical=A Normal=B Deferred=C")
    
    assert converter.priority_map == {"Critical": "A", "Normal": "B", "Deferred": "C"}
    
    output = converter.convert()
    assert "#+GITHUB_PRIORITY_MAP: Critical=A Normal=B Deferred=C" in output


def test_no_priority_field():
    """Test that no priority headers are generated when no priority field exists."""
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [{"name": "Todo"}, {"name": "Done"}]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "#+PRIORITIES:" not in output
    assert "#+GITHUB_PRIORITY_MAP:" not in output


# ============================================================================
# Status Colors Tests
# ============================================================================

def test_status_colors_extraction():
    """Test that status colors are extracted from project fields."""
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [
                        {"name": "Todo", "color": "YELLOW"},
                        {"name": "In Progress", "color": "PURPLE"},
                        {"name": "Done", "color": "GREEN"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data)
    
    assert converter.status_colors == {"TODO": "YELLOW", "STRT": "PURPLE", "DONE": "GREEN"}
    
    output = converter.convert()
    assert "#+GITHUB_STATUS_COLORS:" in output
    assert "TODO=YELLOW" in output
    assert "STRT=PURPLE" in output
    assert "DONE=GREEN" in output


def test_status_colors_persistence():
    """Test that status colors are read from file."""
    import tempfile
    
    org_content = """#+TITLE: Test
#+GITHUB_STATUS_COLORS: TODO=YELLOW STRT=PURPLE DONE=GREEN
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.org', delete=False) as f:
        f.write(org_content)
        f.flush()
        
        config = extract_config_from_org(f.name)
        assert config.get('status_colors') == "TODO=YELLOW STRT=PURPLE DONE=GREEN"
        
        import os
        os.unlink(f.name)


def test_no_status_colors_when_missing():
    """Test that no color header is generated when colors not in project data."""
    project_data = {
        "items": {"nodes": []},
        "fields": {
            "nodes": [
                {
                    "name": "Status",
                    "options": [
                        {"name": "Todo"},  # No color field
                        {"name": "Done"}
                    ]
                }
            ]
        }
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "#+GITHUB_STATUS_COLORS:" not in output


# --- Markdown to Org Conversion Tests ---

def test_markdown_links_to_org():
    """Test that Markdown links are converted to Org links."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "Check out [this link](https://example.com) for details.",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "[[https://example.com][this link]]" in output
    assert "[this link](https://example.com)" not in output


def test_markdown_bold_italic_to_org():
    """Test bold and italic conversion."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "This is **bold** and *italic* and __also bold__ and _also italic_.",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    # Bold: **text** → *text*
    assert "*bold*" in output
    assert "**bold**" not in output
    
    # Italic: *text* → /text/
    assert "/italic/" in output
    
    # __text__ → *text*
    assert "*also bold*" in output
    
    # _text_ → /text/
    assert "/also italic/" in output


def test_markdown_code_to_org():
    """Test inline code and code blocks."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "Use `print()` function.\n\n```python\ndef hello():\n    print('hi')\n```",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    # Inline code: `code` → =code=
    assert "=print()=" in output
    assert "`print()`" not in output
    
    # Code block: ```python → #+BEGIN_SRC python
    assert "#+BEGIN_SRC python" in output
    assert "#+END_SRC" in output
    assert "```python" not in output


def test_markdown_strikethrough_to_org():
    """Test strikethrough conversion."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "This is ~~deleted~~ text.",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "+deleted+" in output
    assert "~~deleted~~" not in output


def test_markdown_blockquote_to_org():
    """Test blockquote conversion."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "Someone said:\n> This is a quote\n> with multiple lines",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert ": This is a quote" in output
    assert ": with multiple lines" in output
    assert "> This is a quote" not in output


def test_markdown_image_to_org():
    """Test image link conversion."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "See this screenshot:\n![screenshot](https://example.com/img.png)",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    # Images become plain links in Org
    assert "[[https://example.com/img.png]]" in output
    assert "![screenshot]" not in output


def test_markdown_checkbox_to_org():
    """Test checkbox conversion (uppercase X)."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "- [x] Done task\n- [ ] Pending task",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "- [X] Done task" in output
    assert "- [ ] Pending task" in output
    assert "- [x]" not in output


def test_markdown_github_mentions_to_org():
    """Test GitHub @mentions become profile links."""
    project_data = {
        "title": "Test",
        "items": {"nodes": [{
            "id": "1",
            "type": "ISSUE",
            "content": {
                "title": "Test",
                "body": "Thanks @rowlandm and @some-user for the help!",
                "number": 1,
                "url": "http://example.com",
                "assignees": {"nodes": []},
                "labels": {"nodes": []}
            },
            "fieldValues": {"nodes": [{"name": "Todo", "field": {"name": "Status"}}]}
        }]},
        "fields": {"nodes": [{"name": "Status", "options": [{"name": "Todo"}]}]}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "[[https://github.com/rowlandm][@rowlandm]]" in output
    assert "[[https://github.com/some-user][@some-user]]" in output
    assert "@rowlandm" not in output.replace("[@rowlandm]", "")  # No bare @mentions


def test_local_variables_block_default():
    """Test that local variables block is added by default."""
    project_data = {
        "title": "Test",
        "items": {"nodes": []},
        "fields": {"nodes": []}
    }
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "* COMMENT Local Variables" in output
    assert "# Local Variables:" in output
    assert "# eval: (project-to-org-mode 1)" in output
    assert "# End:" in output


def test_local_variables_block_disabled():
    """Test that local variables block can be disabled."""
    project_data = {
        "title": "Test",
        "items": {"nodes": []},
        "fields": {"nodes": []}
    }
    
    converter = OrgConverter(project_data, add_local_variables=False)
    output = converter.convert()
    
    assert "* COMMENT Local Variables" not in output
    assert "# Local Variables:" not in output
    assert "# eval: (project-to-org-mode 1)" not in output
