import pytest
import time
import json
from project_to_org.github_client import fetch_project_items, get_github_token
from project_to_org.org_converter import OrgConverter
# conftest is automatically loaded by pytest, but we need to import helpers if we want to use them directly
# However, best practice is to put helpers in a separate module or use fixtures.
# For now, let's just duplicate the imports or fix the path.
# Since we are running from root, 'tests.conftest' should work if tests is a package (has __init__.py)
# Let's create __init__.py in tests/

from tests.conftest import run_gh, run_gh_text, OWNER, REPO

@pytest.fixture
def temp_project():
    """Fixture to create and destroy a temporary project."""
    timestamp = int(time.time())
    title = f"Edge Case Test {timestamp}"
    print(f"Creating temp project: {title}")
    project = run_gh(["project", "create", "--owner", OWNER, "--title", title])
    
    yield project
    
    print(f"Deleting temp project {project['number']}")
    run_gh_text(["project", "delete", str(project["number"]), "--owner", OWNER])

@pytest.fixture(scope="module")
def persistent_project_info():
    """Get info about the persistent project without modifying it (cleanup is done in test_integration)."""
    PERSISTENT_PROJECT_TITLE = "Integration Test Project (Persistent)"
    projects = run_gh(["project", "list", "--owner", OWNER])
    project = next((p for p in projects["projects"] if p["title"] == PERSISTENT_PROJECT_TITLE), None)
    if not project:
        pytest.skip("Persistent project not found. Run test_integration.py first?")
    return project

@pytest.mark.integration
def test_empty_project(temp_project):
    """Test syncing an empty project."""
    token = get_github_token()
    project_data = fetch_project_items("user", OWNER, temp_project["number"], token)
    
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    assert "#+TITLE:" in output
    assert "* TODO" not in output
    assert "#+GITHUB_STATUS_MAP" in output

@pytest.mark.integration
def test_custom_statuses(temp_project):
    """Test syncing a project with custom statuses."""
    project_number = temp_project["number"]
    print(f"Testing custom statuses on project {project_number}")
    
    # 1. Find default Status field
    print("Finding Status field...")
    fields = run_gh(["project", "field-list", str(project_number), "--owner", OWNER])
    status_field = next((f for f in fields["fields"] if f["name"] == "Status"), None)
    assert status_field, "Status field not found"
    
    # 2. Add "Triage" option to the existing Status field
    print("Adding 'Triage' option...")
    current_options = status_field.get("options", [])
    
    # Construct options list for mutation. 
    # We preserve existing options to avoid errors or data loss.
    options_input = []
    for opt in current_options:
        # We use a default color for existing ones to keep it simple
        options_input.append(f'{{ name: "{opt["name"]}", color: GRAY, description: "" }}')
    
    options_input.append('{ name: "Triage", color: RED, description: "" }')
    options_str = ", ".join(options_input)
    
    query = f'''
    mutation {{
      updateProjectV2Field(input: {{
        fieldId: "{status_field['id']}",
        singleSelectOptions: [{options_str}]
      }}) {{
        projectV2Field {{
          ... on ProjectV2SingleSelectField {{
            id
            options {{
              id
              name
            }}
          }}
        }}
      }}
    }}
    '''
    
    # Use run_gh to get JSON response
    result = run_gh(["api", "graphql", "-f", f"query={query}"])
    
    # Parse result to get new option ID
    new_field_data = result["data"]["updateProjectV2Field"]["projectV2Field"]
    triage_option = next(o for o in new_field_data["options"] if o["name"] == "Triage")
    print(f"Created 'Triage' option with ID: {triage_option['id']}")
    
    # 3. Create a draft issue and set status to "Triage"
    print("Creating item...")
    item = run_gh([
        "project", "item-create", str(project_number), 
        "--owner", OWNER, 
        "--title", "Custom Status Item", 
        "--body", "Body"
    ])
    
    print(f"Setting item {item['id']} status to Triage...")
    run_gh_text([
        "project", "item-edit", 
        "--id", item["id"], 
        "--project-id", temp_project["id"], 
        "--field-id", status_field["id"], 
        "--single-select-option-id", triage_option["id"]
    ])
    
    # 4. Sync
    print("Syncing...")
    time.sleep(2) # Wait for consistency
    token = get_github_token()
    project_data = fetch_project_items("user", OWNER, project_number, token)
    converter = OrgConverter(project_data)
    output = converter.convert()
    
    # 5. Verify
    # Heuristics: Triage -> TRIAGE (fallback)
    print("Verifying output...")
    assert "* TRIAGE Custom Status Item" in output
    assert "Triage:TRIAGE" in output or '"Triage":TRIAGE' in output

@pytest.mark.integration
def test_draft_issue_sync(persistent_project_info):
    """Test syncing a draft issue."""
    project_number = persistent_project_info["number"]
    
    # Create draft issue
    item = run_gh([
        "project", "item-create", str(project_number), 
        "--owner", OWNER, 
        "--title", "Draft Issue Test", 
        "--body", "Draft Body"
    ])
    
    try:
        time.sleep(2)
        token = get_github_token()
        project_data = fetch_project_items("user", OWNER, project_number, token)
        converter = OrgConverter(project_data)
        output = converter.convert()
        
        assert "* TODO Draft Issue Test" in output
        assert "Draft Body" in output
        assert f":ID: {item['id']}" in output
        
    finally:
        # Cleanup
        run_gh_text(["project", "item-delete", str(project_number), "--owner", OWNER, "--id", item["id"]])

@pytest.mark.integration
def test_complex_content(persistent_project_info):
    """Test syncing complex markdown content."""
    project_number = persistent_project_info["number"]
    
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
    
    # Create issue
    issue_url = run_gh_text(["issue", "create", "--repo", REPO, "--title", "Complex Content Test", "--body", body])
    item = run_gh(["project", "item-add", str(project_number), "--owner", OWNER, "--url", issue_url])
    
    try:
        time.sleep(2)
        token = get_github_token()
        project_data = fetch_project_items("user", OWNER, project_number, token)
        converter = OrgConverter(project_data)
        output = converter.convert()
        
        assert "* TODO Complex Content Test" in output
        assert "print(\"Hello\")" in output
        assert "> Blockquote" in output
        
    finally:
        # Cleanup
        run_gh_text(["project", "item-delete", str(project_number), "--owner", OWNER, "--id", item["id"]])
        issue_number = issue_url.split("/")[-1]
        run_gh_text(["issue", "close", issue_number, "--repo", REPO])

@pytest.mark.integration
def test_status_exclusion(persistent_project_info):
    """Test excluding statuses."""
    project_number = persistent_project_info["number"]
    
    # Create two draft items
    item1 = run_gh(["project", "item-create", str(project_number), "--owner", OWNER, "--title", "Keep Me"])
    item2 = run_gh(["project", "item-create", str(project_number), "--owner", OWNER, "--title", "Exclude Me"])
    
    # Set item2 to "Done"
    fields = run_gh(["project", "field-list", str(project_number), "--owner", OWNER])
    status_field = next(f for f in fields["fields"] if f["name"] == "Status")
    done_option = next(o for o in status_field["options"] if o["name"] == "Done")
    
    run_gh_text([
        "project", "item-edit", 
        "--id", item2["id"], 
        "--project-id", persistent_project_info["id"], 
        "--field-id", status_field["id"], 
        "--single-select-option-id", done_option["id"]
    ])
    
    try:
        time.sleep(2)
        token = get_github_token()
        project_data = fetch_project_items("user", OWNER, project_number, token)
        
        # Exclude "Done"
        converter = OrgConverter(project_data, exclude_statuses=["Done"])
        output = converter.convert()
        
        assert "Keep Me" in output
        assert "Exclude Me" not in output
        assert "#+GITHUB_EXCLUDE_STATUSES: Done" in output
        
    finally:
        run_gh_text(["project", "item-delete", str(project_number), "--owner", OWNER, "--id", item1["id"]])
        run_gh_text(["project", "item-delete", str(project_number), "--owner", OWNER, "--id", item2["id"]])
