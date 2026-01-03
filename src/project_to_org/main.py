import os
import sys
import argparse
import json
from dotenv import load_dotenv
from project_to_org.github_client import get_github_token, parse_project_url, fetch_project_items
from project_to_org.org_converter import OrgConverter, extract_config_from_org

def main():
    load_dotenv()
    
    parser = argparse.ArgumentParser(description="Sync GitHub Projects to Org-mode")
    parser.add_argument("--org-file", help="Path to the Org file to sync", required=False)
    parser.add_argument("--project-url", help="GitHub Project URL", required=True)
    parser.add_argument("--exclude-statuses", help="List of statuses to exclude", nargs="*", default=[])
    parser.add_argument("--status-map", help="Status mapping string (e.g. 'Todo=TODO \"In Progress\"=STRT')", required=False)
    
    args = parser.parse_args()
    
    try:
        token = get_github_token()
        owner_type, owner, number = parse_project_url(args.project_url)
        
        # print(f"Fetching project {owner}/{number} ({owner_type})...")
        project_data = fetch_project_items(owner_type, owner, number, token)
        
        # Extract existing config if file exists
        existing_config = {}
        if args.org_file:
            existing_config = extract_config_from_org(args.org_file)
            
        # Determine status map: CLI > File > None (Default)
        status_map_to_use = args.status_map or existing_config.get('status_map')
        
        # Determine exclude statuses: CLI (if not default empty) > File > CLI Default
        # Note: args.exclude_statuses is a list, default []
        # If user passed --exclude-statuses, it will be non-empty (or empty list if they passed nothing, but default is [])
        # Wait, if user explicitly passes --exclude-statuses with nothing, it might be empty list.
        # But if they don't pass it, it's also empty list.
        # We can't easily distinguish "user passed nothing" vs "user didn't pass flag" with argparse default=[]
        # But usually CLI overrides file.
        # If args.exclude_statuses is NOT empty, use it.
        # If it IS empty, check file.
        # If file has it, use it.
        # Else use empty.
        
        exclude_statuses_to_use = args.exclude_statuses
        if not exclude_statuses_to_use and existing_config.get('exclude_statuses'):
            exclude_statuses_to_use = existing_config.get('exclude_statuses')
        
        converter = OrgConverter(
            project_data, 
            project_url=args.project_url, 
            exclude_statuses=exclude_statuses_to_use,
            status_map_str=status_map_to_use
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
