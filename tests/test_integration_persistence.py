import pytest
import time
import tempfile
import os
import subprocess
from tests.conftest import run_gh, OWNER

def run_sync(args):
    """Run the project-to-org Python script directly."""
    cmd = ["uv", "run", "python", "-m", "project_to_org.main"] + args
    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    return result.stdout.strip()

@pytest.fixture(scope="function")
def persistent_project_info():
    """Get info about the persistent project without creating/destroying it."""
    PERSISTENT_PROJECT_TITLE = "Integration Test Project (Persistent)"
    
    print(f"Looking for project: {PERSISTENT_PROJECT_TITLE}")
    projects = run_gh(["project", "list", "--owner", OWNER])
    project = next((p for p in projects["projects"] if p["title"] == PERSISTENT_PROJECT_TITLE), None)
    
    if not project:
        pytest.skip("Persistent project not found. Run test_integration.py first.")
    
    return {
        "project_number": project["number"],
        "project_id": project["id"],
        "project_title": PERSISTENT_PROJECT_TITLE
    }

@pytest.mark.integration
def test_status_map_persistence(persistent_project_info):
    """Test that a user-modified status map persists across syncs."""
    project_number = persistent_project_info["project_number"]
    project_url = f"https://github.com/users/{OWNER}/projects/{project_number}"
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.org', delete=False) as tmp:
        org_file = tmp.name
    
    try:
        # Step 1: First sync
        print("Step 1: Initial sync...")
        result = run_sync([
            "--project-url", project_url,
            "--org-file", org_file
        ])
        print(f"First sync result: {result}")
        
        # Read the file
        with open(org_file, 'r') as f:
            original_content = f.read()
        
        print(f"Original content:\n{original_content}")
        
        # Verify original status map
        assert "#+GITHUB_STATUS_MAP:" in original_content
        assert "Todo:TODO" in original_content or '"Todo":TODO' in original_content
        
        # Step 2: Modify the status map
        print("Step 2: Modifying status map...")
        modified_content = original_content.replace("Todo:TODO", "Todo:MUSTDO")
        with open(org_file, 'w') as f:
            f.write(modified_content)
        
        # Verify modification
        with open(org_file, 'r') as f:
            check_content = f.read()
        assert "Todo:MUSTDO" in check_content
        print(f"Modified content verified:\n{check_content}")
        
        # Step 3: Second sync
        print("Step 3: Second sync...")
        time.sleep(1)  # Give a moment for file system
        result = run_sync([
            "--project-url", project_url,
            "--org-file", org_file
        ])
        print(f"Second sync result: {result}")
        
        # Step 4: Verify the custom map is preserved
        with open(org_file, 'r') as f:
            final_content = f.read()
        
        print(f"Final content:\n{final_content}")
        
        # This should pass if persistence works
        assert "Todo:MUSTDO" in final_content, "Custom status map was not preserved!"
        
    finally:
        if os.path.exists(org_file):
            os.remove(org_file)
