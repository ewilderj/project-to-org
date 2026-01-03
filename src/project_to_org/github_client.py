import os
import subprocess
import requests
import re

def get_github_token():
    """Retrieve GitHub token from environment or gh CLI."""
    token = os.getenv("GITHUB_TOKEN")
    if token:
        return token
    
    try:
        result = subprocess.run(
            ["gh", "auth", "token"], 
            capture_output=True, 
            text=True, 
            check=True
        )
        return result.stdout.strip()
    except subprocess.CalledProcessError:
        raise RuntimeError("Could not retrieve GitHub token. Please set GITHUB_TOKEN or login with `gh auth login`.")
    except FileNotFoundError:
        raise RuntimeError("`gh` CLI not found. Please install it or set GITHUB_TOKEN.")

def parse_project_url(url):
    """Parse GitHub Project URL to get owner type, owner name, and project number."""
    # Matches https://github.com/users/USERNAME/projects/NUMBER
    user_match = re.match(r"https://github\.com/users/([^/]+)/projects/(\d+)", url)
    if user_match:
        return "user", user_match.group(1), int(user_match.group(2))
    
    # Matches https://github.com/orgs/ORGNAME/projects/NUMBER
    org_match = re.match(r"https://github\.com/orgs/([^/]+)/projects/(\d+)", url)
    if org_match:
        return "organization", org_match.group(1), int(org_match.group(2))
        
    raise ValueError(f"Invalid GitHub Project URL: {url}")

def fetch_project_items(owner_type, owner, number, token):
    """Fetch items from a GitHub Project V2."""
    url = "https://api.github.com/graphql"
    headers = {"Authorization": f"Bearer {token}"}
    
    # Construct the query based on owner type
    owner_query_part = ""
    if owner_type == "user":
        owner_query_part = f'user(login: "{owner}")'
    else:
        owner_query_part = f'organization(login: "{owner}")'
        
    query = f"""
    query {{
      {owner_query_part} {{
        projectV2(number: {number}) {{
          title
          fields(first: 20) {{
            nodes {{
              ... on ProjectV2FieldCommon {{
                name
                dataType
              }}
              ... on ProjectV2SingleSelectField {{
                name
                options {{
                  name
                  color
                }}
              }}
            }}
          }}
          items(first: 100) {{
            nodes {{
              id
              type
              content {{
                ... on Issue {{
                  title
                  body
                  state
                  number
                  url
                  assignees(first: 10) {{
                    nodes {{
                      login
                    }}
                  }}
                  labels(first: 10) {{
                    nodes {{
                      name
                    }}
                  }}
                }}
                ... on DraftIssue {{
                  title
                  body
                }}
              }}
              fieldValues(first: 20) {{
                nodes {{
                  ... on ProjectV2ItemFieldSingleSelectValue {{
                    name
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                  ... on ProjectV2ItemFieldTextValue {{
                    text
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                  ... on ProjectV2ItemFieldDateValue {{
                    date
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                  ... on ProjectV2ItemFieldIterationValue {{
                    title
                    field {{
                      ... on ProjectV2FieldCommon {{
                        name
                      }}
                    }}
                  }}
                }}
              }}
            }}
          }}
        }}
      }}
    }}
    """
    
    response = requests.post(url, json={"query": query}, headers=headers)
    if response.status_code != 200:
        raise RuntimeError(f"GraphQL query failed: {response.status_code} {response.text}")
        
    data = response.json()
    if "errors" in data:
        raise RuntimeError(f"GraphQL errors: {data['errors']}")
        
    # Navigate the response structure dynamically based on owner_type
    root = data["data"]["user"] if owner_type == "user" else data["data"]["organization"]
    if not root or not root["projectV2"]:
        raise RuntimeError(f"Project not found: {owner}/{number}")
        
    return root["projectV2"]
