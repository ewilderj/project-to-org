import pytest
import subprocess
import json
import os

OWNER = "ewilderj"
REPO = "ewilderj/project-to-org-sandbox"

def run_gh(args):
    """Run a gh command and return the JSON output."""
    cmd = ["gh"] + args
    # gh api outputs JSON by default and doesn't support --format
    if args[0] != "api":
        cmd += ["--format", "json"]
        
    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    return json.loads(result.stdout)

def run_gh_text(args):
    """Run a gh command and return text output."""
    cmd = ["gh"] + args
    result = subprocess.run(cmd, capture_output=True, text=True, check=True)
    return result.stdout.strip()
