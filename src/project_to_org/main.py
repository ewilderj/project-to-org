import os
import sys
import argparse
import json
from dotenv import load_dotenv
from project_to_org.github_client import get_github_token, parse_project_url, fetch_project_items
from project_to_org.org_converter import OrgConverter

def main():
    load_dotenv()
    
    parser = argparse.ArgumentParser(description="Sync GitHub Projects to Org-mode")
    parser.add_argument("--org-file", help="Path to the Org file to sync", required=False)
    parser.add_argument("--project-url", help="GitHub Project URL", required=True)
    parser.add_argument("--exclude-statuses", help="List of statuses to exclude", nargs="*", default=[])
    parser.add_argument("--status-map", help="Status mapping string (e.g. 'Todo:TODO \"In Progress\":STRT')", required=False)
    
    args = parser.parse_args()
    
    try:
        token = get_github_token()
        owner_type, owner, number = parse_project_url(args.project_url)
        
        # print(f"Fetching project {owner}/{number} ({owner_type})...")
        project_data = fetch_project_items(owner_type, owner, number, token)
        
        converter = OrgConverter(
            project_data, 
            project_url=args.project_url, 
            exclude_statuses=args.exclude_statuses,
            status_map_str=args.status_map
        )
        org_content = converter.convert()
        
        if args.org_file:
            with open(args.org_file, "w") as f:
                f.write(org_content)
            print(f"Successfully synced to {args.org_file}")
        else:
            print(org_content)
        
    except Exception as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    
if __name__ == "__main__":
    main()
