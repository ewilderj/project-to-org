import pytest
import tempfile
import os
import subprocess

def run_sync(args):
    """Run the project-to-org Python script directly."""
    cmd = ["uv", "run", "python", "-m", "project_to_org.main"] + args
    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    return result.stdout.strip()

@pytest.mark.integration
def test_waiting_for_godot_persistence():
    """Reproduce the exact user scenario with Waiting For -> GODOT."""
    
    project_url = "https://github.com/users/ewilderj/projects/1"
    
    # Create a temp file with the user's MODIFIED content
    before_content = """#+TITLE: ewilderj work
#+GITHUB_PROJECT_URL: https://github.com/users/ewilderj/projects/1
#+GITHUB_EXCLUDE_STATUSES: Done
#+GITHUB_STATUS_MAP: Todo:TODO "In Progress":STRT "Waiting For":GODOT Someday:WAIT Done:DONE
#+DATE: 2026-01-02
#+TODO: TODO STRT GODOT WAIT | DONE
#+STARTUP: show2levels

* Local Variables :noexport:
# Local Variables:
# org-tidy-mode: nil
# End:
"""
    
    with tempfile.NamedTemporaryFile(mode='w', suffix='.org', delete=False) as tmp:
        org_file = tmp.name
        tmp.write(before_content)
    
    try:
        print(f"Testing with file: {org_file}")
        print(f"Before sync:\n{before_content}")
        
        # Run sync
        result = run_sync([
            "--project-url", project_url,
            "--org-file", org_file
        ])
        print(f"Sync result: {result}")
        
        # Read the result
        with open(org_file, 'r') as f:
            after_content = f.read()
        
        print(f"After sync:\n{after_content}")
        
        # Verify GODOT is preserved
        assert "GODOT" in after_content, "GODOT mapping was not preserved!"
        assert "WAITING_FOR" not in after_content, "WAITING_FOR should not appear - we changed it to GODOT!"
        
    finally:
        if os.path.exists(org_file):
            os.remove(org_file)
