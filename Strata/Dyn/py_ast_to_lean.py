#!/usr/bin/env python3
import sys
import json

# Converts a Python AST.json file into Lean-compatible.json file
# The Lean-compatible version differs in that inductive type enums become tagged
# Python AST uses "_type" field instead of "type"

def tag_statement_enums(j):
    """
    Tags Statement Enums. We support:
    - Assign
    - AugAssign
    - FunctionDef
    - Global
    - Return
    - While
    """
    def tag_statement_enums_on_children(n):
        res = {}
        for (k, v) in n.items():
            res[k] = tag_statement_enums(v)
        return res
    if isinstance(j, dict):
        if "_type" in j and j["_type"] == "Assign":
            return {"Assign": tag_statement_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "AugAssign":
            return {"AugAssign": tag_statement_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "FunctionDef":
            return {"FunctionDef": tag_statement_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "Global":
            return {"Global": tag_statement_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "Return":
            return {"Return": tag_statement_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "While":
            return {"While": tag_statement_enums_on_children(j)}
        return tag_statement_enums_on_children(j)
    if isinstance(j, list):
        return [tag_statement_enums(n) for n in j]
    else:
        return j
    
def tag_expression_enums(j):
    """
    Tags Expression Enums. We support:
    - Name
    - Constant
    - BinOp
    - Compare
    """
    def tag_expression_enums_on_children(n):
        res = {}
        for (k, v) in n.items():
            res[k] = tag_expression_enums(v)
        return res
    if isinstance(j, dict):
        if "_type" in j and j["_type"] == "Name":
            return {"Name": tag_expression_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "Constant":
            return {"Constant": tag_expression_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "BinOp":
            return {"BinOp": tag_expression_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "Compare":
            return {"Compare": tag_expression_enums_on_children(j)}
        return tag_expression_enums_on_children(j)
    if isinstance(j, list):
        return [tag_expression_enums(n) for n in j]
    else:
        return j


def tag_context_enums(j):
    """
    Tags Context Enums. We support:
    - Load
    - Store
    """
    def tag_context_enums_on_children(n):
        res = {}
        for (k, v) in n.items():
            res[k] = tag_context_enums(v)
        return res
    if isinstance(j, dict):
        if "_type" in j and j["_type"] == "Load":
            return {"Load": tag_context_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "Store":
            return {"Store": tag_context_enums_on_children(j)}
        return tag_context_enums_on_children(j)
    if isinstance(j, list):
        return [tag_context_enums(n) for n in j]
    else:
        return j

def tag_operator_enums(j):
    """
    Tags Operator Enums. We support:
    - Add
    - Mult
    - Lt
    """
    def tag_operator_enums_on_children(n):
        res = {}
        for (k, v) in n.items():
            res[k] = tag_operator_enums(v)
        return res
    if isinstance(j, dict):
        if "_type" in j and j["_type"] == "Add":
            return {"Add": tag_operator_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "Mult":
            return {"Mult": tag_operator_enums_on_children(j)}
        elif "_type" in j and j["_type"] == "Lt":
            return {"Lt": tag_operator_enums_on_children(j)}
        return tag_operator_enums_on_children(j)
    if isinstance(j, list):
        return [tag_operator_enums(n) for n in j]
    else:
        return j


def modify(j):
    """
    Transform Python's AST.json into Lean-compatible JSON
    """
    return tag_statement_enums(tag_expression_enums(tag_context_enums(tag_operator_enums(j))))

def main():
    if len(sys.argv) != 2:
        print("Usage: python py_ast_to_lean.py <filename>")
        sys.exit(1)

    filename = sys.argv[1]
    try:
        # Read the file
        with open(filename, 'r') as file:
            s = file.read()
            j = json.loads(s)
            modified_j = modify(j)
            print(json.dumps(modified_j, indent=2))
            
    except FileNotFoundError:
        print(f"Error: File '{filename}' not found.")
        sys.exit(1)    

if __name__ == "__main__":
    main()
