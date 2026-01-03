#!/usr/bin/env python3
"""Check parentheses balance in an elisp file."""

def check_parens(filepath):
    with open(filepath, 'r') as f:
        content = f.read()
    
    stack = []
    line_num = 1
    col_num = 1
    in_string = False
    in_comment = False
    escape_next = False
    
    for i, char in enumerate(content):
        if char == '\n':
            line_num += 1
            col_num = 1
            in_comment = False
            continue
        
        col_num += 1
        
        # Handle comments
        if not in_string and char == ';':
            in_comment = True
            continue
        
        if in_comment:
            continue
        
        # Handle strings
        if char == '"' and not escape_next:
            in_string = not in_string
            continue
        
        if char == '\\' and in_string:
            escape_next = not escape_next
            continue
        else:
            escape_next = False
        
        if in_string:
            continue
        
        # Check parens
        if char == '(':
            stack.append((line_num, col_num))
        elif char == ')':
            if not stack:
                print(f"ERROR: Extra closing paren at line {line_num}, col {col_num}")
                return False
            stack.pop()
    
    if stack:
        print(f"ERROR: {len(stack)} unclosed opening paren(s)")
        for line, col in stack[-5:]:  # Show last 5
            print(f"  Unclosed '(' at line {line}, col {col}")
        return False
    
    print("✓ Parentheses are balanced!")
    return True

if __name__ == '__main__':
    import sys
    check_parens(sys.argv[1] if len(sys.argv) > 1 else 'project-to-org.el')
