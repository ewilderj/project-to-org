import pytest
from project_to_org.org_converter import OrgConverter

def test_convert_complex_content():
    """Test converting complex markdown content."""
    body = """
# Heading 1
## Heading 2
- List item 1
- List item 2

```python
print("Hello")
```

> Blockquote
    """
    mock_data = {
        "title": "Test Project",
        "items": {
            "nodes": [
                {
                    "id": "1",
                    "type": "ISSUE",
                    "content": {
                        "title": "Complex Content",
                        "body": body,
                        "number": 1,
                        "url": "http://github.com/issue/1"
                    },
                    "fieldValues": {"nodes": []}
                }
            ]
        }
    }
    
    converter = OrgConverter(mock_data)
    output = converter.convert()
    
    assert "* TODO Complex Content" in output
    assert "print(\"Hello\")" in output
    assert "> Blockquote" in output
    assert "# Heading 1" in output

def test_convert_draft_issue():
    """Test converting a draft issue."""
    mock_data = {
        "title": "Test Project",
        "items": {
            "nodes": [
                {
                    "id": "draft_1",
                    "type": "DRAFT_ISSUE",
                    "content": {
                        "title": "Draft Issue",
                        "body": "Draft Body"
                    },
                    "fieldValues": {"nodes": []}
                }
            ]
        }
    }
    
    converter = OrgConverter(mock_data)
    output = converter.convert()
    
    assert "* TODO Draft Issue" in output
    assert "Draft Body" in output
    assert ":ID: draft_1" in output
    # Draft issues don't have number or URL usually in the content object structure we mocked, 
    # or at least our converter handles missing fields gracefully.
    assert ":ISSUE_NUMBER:" not in output

def test_convert_empty_project():
    """Test converting an empty project."""
    mock_data = {
        "title": "Empty Project",
        "items": {"nodes": []},
        "fields": {"nodes": []}
    }
    
    converter = OrgConverter(mock_data)
    output = converter.convert()
    
    assert "#+TITLE: Empty Project" in output
    assert "* TODO" not in output
    # Should still have default status map if no fields found
    assert "#+GITHUB_STATUS_MAP" in output
