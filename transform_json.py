#!/usr/bin/env python3

import json
import sys

def transform_node(node):
    """Transform a Python AST node to Lean's expected format"""
    if not isinstance(node, dict):
        return node
    
    if "_type" not in node:
        return node
    
    node_type = node["_type"]
    
    # Create a copy with all fields transformed
    transformed = {}
    for key, value in node.items():
        if isinstance(value, dict):
            transformed[key] = transform_node(value)
        elif isinstance(value, list):
            transformed[key] = [transform_node(item) for item in value]
        else:
            transformed[key] = value
    
    # For inductive types, wrap in constructor format
    # But keep _type field for structures that extend BaseNode
    if node_type in ["Module"]:  # Top-level structures
        return transformed
    elif node_type in ["Assign", "AugAssign", "FunctionDef", "Global", "Return", "While"]:  # Statement types
        return {node_type: transformed}
    elif node_type in ["Name", "Constant", "BinOp", "Compare"]:  # Expression types
        return {node_type: transformed}
    elif node_type in ["Load", "Store"]:  # Context types
        return {node_type: transformed}
    elif node_type in ["Add", "Mult", "Lt"]:  # Operator types
        return {node_type: transformed}
    else:
        # For other structures, keep as-is
        return transformed

def main():
    if len(sys.argv) != 2:
        print("Usage: python3 transform_json.py input.json")
        sys.exit(1)
    
    with open(sys.argv[1], 'r') as f:
        data = json.load(f)
    
    transformed = transform_node(data)
    print(json.dumps(transformed, indent=2))

if __name__ == "__main__":
    main()
