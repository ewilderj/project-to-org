import pytest
import time
from project_to_org.github_client import fetch_project_items, get_github_token
from project_to_org.org_converter import OrgConverter
from tests.conftest import run_gh, run_gh_text, OWNER, REPO

@pytest.fixture(scope="module")
def integration_project():
    # Constants
    PERSISTENT_PROJECT_TITLE = "Integration Test Project (Persistent)"
    
    # 1. Find or Create Project
    print(f"Looking for project: {PERSISTENT_PROJECT_TITLE}")
    projects = run_gh(["project", "list", "--owner", OWNER])
    project = next((p for p in projects["projects"] if p["title"] == PERSISTENT_PROJECT_TITLE), None)
    
    if not project:
        print("Project not found, creating...")
        project = run_gh(["project", "create", "--owner", OWNER, "--title", PERSISTENT_PROJECT_TITLE])
    else:
        print(f"Found project {project['number']}")

    project_number = project["number"]
    project_id = project["id"]
    
    # 2. Cleanup (Start of test)
    print("Cleaning up project items...")
    items = run_gh(["project", "item-list", str(project_number), "--owner", OWNER, "--limit", "100"])
    for item in items["items"]:
        print(f"Deleting item {item['id']}")
        run_gh_text(["project", "item-delete", str(project_number), "--owner", OWNER, "--id", item["id"]])
        
        # Try to close the issue if it's an issue
        content = item.get("content", {})
        if content.get("type") == "Issue":
             # We can't easily get the repo from here without parsing URL or more queries
             # But we can try to close issues with "Integration Issue" in title in the repo separately
             pass

    # Cleanup old integration issues in the repo to prevent clutter
    print("Cleaning up old integration issues...")
    # gh issue list uses --json not --format json
    issues_json = run_gh_text(["issue", "list", "--repo", REPO, "--state", "open", "--search", "Integration Issue", "--limit", "100", "--json", "number"])
    issues = json.loads(issues_json)
    for issue in issues:
        print(f"Closing old issue {issue['number']}")
        run_gh_text(["issue", "close", str(issue['number']), "--repo", REPO])

    # 3. Setup New Test Data
    timestamp = int(time.time())
    issue_title = f"Integration Issue {timestamp}"
    print(f"Creating issue: {issue_title}")
    issue_url = run_gh_text(["issue", "create", "--repo", REPO, "--title", issue_title, "--body", "Test Body"])
    print(f"Created issue: {issue_url}")
    issue_number = int(issue_url.split("/")[-1])
    
    print(f"Adding issue to project {project_number}")
    item = run_gh(["project", "item-add", str(project_number), "--owner", OWNER, "--url", issue_url])
    
    data = {
        "project_number": project_number,
        "project_id": project_id,
        "project_title": PERSISTENT_PROJECT_TITLE,
        "issue_url": issue_url,
        "issue_title": issue_title,
        "issue_number": issue_number,
        "item_id": item["id"]
    }
    
    yield data
    
    # 4. Teardown (Do nothing, leave for inspection)
    print("Test complete. Project and issue left for inspection.")


@pytest.mark.integration
def test_integration_sync(integration_project):
    token = get_github_token()
    project_number = integration_project["project_number"]
    
    # Fetch data
    print("Fetching project data...")
    # Wait for consistency
    time.sleep(2) 
    
    project_data = fetch_project_items("user", OWNER, project_number, token)
    
    assert project_data["title"] == integration_project["project_title"]
    
    # Convert
    converter = OrgConverter(project_data)
    org_output = converter.convert()
    
    # Verify
    assert f"* TODO {integration_project['issue_title']}" in org_output
    assert f":ID: {integration_project['item_id']}" in org_output
    assert f":URL: {integration_project['issue_url']}" in org_output
    assert "#+GITHUB_STATUS_MAP" in org_output
    
    # Verify heuristics worked (Todo -> TODO)
    # Note: New projects usually have "Todo", "In Progress", "Done"
    assert "Todo=TODO" in org_output or '"Todo"=TODO' in org_output
