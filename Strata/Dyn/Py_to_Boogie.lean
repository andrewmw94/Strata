import Strata.Dyn.py_ast

-- Helper function to translate constant values
def translate_constant_value (cv: ConstantValue) : String :=
  match cv with
  | ConstantValue.num n => toString n
  | ConstantValue.str s => s!"\"{s}\""
  | ConstantValue.bool b => if b then "true" else "false"
  | ConstantValue.null => "null"

-- Helper function to translate operators
def translate_operator (op: Operator) : String :=
  match op with
  | Operator.Add _ => "+"
  | Operator.Mult _ => "*"
  | Operator.Lt _ => "<"

-- Helper function to translate variable names
def translate_name (name: Name) : String :=
  name.id

-- Translate function arguments
def translate_arg (arg: Arg) : String :=
  s!"{arg.arg} : int"

def translate_args (args: Arguments) : String :=
  ", ".intercalate (args.args.map translate_arg).toList

-- Helper function to collect local variable names from function body
def collect_local_vars (stmts: Array Statement) : List String :=
  let rec collect_from_stmt (stmt: Statement) : List String :=
    match stmt with
    | Statement.Assign assign =>
        match assign.targets[0]? with
        | some (Expression.Name name) => [name.id]
        | _ => []
    | Statement.AugAssign augassign =>
        match augassign.target with
        | Expression.Name name => [name.id]
        | _ => []
    | _ => []
  
  -- Simple approach: just collect from top-level statements
  let all_vars := stmts.toList.foldl (fun acc stmt => acc ++ collect_from_stmt stmt) []
  -- Filter out function parameters and return variable
  all_vars.filter (fun v => v ≠ "ret")

mutual
  -- Translate expressions
  partial def translate_expression (expr: Expression) : String :=
    match expr with
    | Expression.Name name => translate_name name
    | Expression.Constant const => translate_constant_value const.value
    | Expression.BinOp binop => 
        let left := translate_expression binop.left
        let right := translate_expression binop.right
        let op := translate_operator binop.op
        s!"({left} {op} {right})"
    | Expression.Compare comp =>
        let left := translate_expression comp.left
        -- For simplicity, handle single comparison for now
        match comp.ops[0]?, comp.comparators[0]? with
        | some op, some right_expr =>
            let op_str := translate_operator op
            let right := translate_expression right_expr
            s!"({left} {op_str} {right})"
        | _, _ => left

  -- Translate statements - now returns Option String
  partial def translate_statement (stmt: Statement) : Option String :=
    match stmt with
    | Statement.Assign assign =>
        -- Handle simple assignment to first target
        match assign.targets[0]? with
        | some (Expression.Name name) =>
            let value := translate_expression assign.value
            some s!"  {name.id} := {value};"
        | some _ => some "  // Unsupported assignment target"
        | none => some "  // Empty assignment"
    | Statement.AugAssign augassign =>
        match augassign.target with
        | Expression.Name name =>
            let value := translate_expression augassign.value
            let op := translate_operator augassign.op
            some s!"  {name.id} := {name.id} {op} {value};"
        | _ => some "  // Unsupported augmented assignment target"
    | Statement.FunctionDef funcdef =>
        some (translate_function funcdef)
    | Statement.Global _ =>
        -- Global statements are handled at module level, skip in function body
        some "  // global declaration"
    | Statement.Return ret =>
        match ret.value with
        | some expr => some s!"  ret := {translate_expression expr};"
        | none => some "  return;"
    | Statement.While _ =>
        -- While loops are unsupported - return none
        none

  -- Translate function definition
  partial def translate_function (funcdef: FunctionDef) : String :=
    let name := funcdef.name
    let args := translate_args funcdef.args
    
    -- Try to translate all statements
    let stmt_results := funcdef.body.map translate_statement
    
    -- Check if any statement is unsupported (returns none)
    let has_unsupported := stmt_results.any (· == none)
    
    if has_unsupported then
      -- Generate comment block for unsupported function
      -- Note: We don't have line number info in the AST, so using placeholder
      s!"// Function with unsupported feature: `{name}:line_num`"
    else
      -- All statements are supported, generate normal function
      let body_stmts := stmt_results.filterMap id
      let body := "\n".intercalate body_stmts.toList
      
      -- Collect local variables from assignments in function body
      let local_vars := collect_local_vars funcdef.body
      let var_decls := if local_vars.isEmpty then "" else
        "  " ++ "\n  ".intercalate (local_vars.map (fun v => s!"var {v}: int;")) ++ "\n"
      
      -- Generate function signature
      let signature := if args.isEmpty then
        s!"procedure {name}() returns (ret : int)"
      else
        s!"procedure {name}({args}) returns (ret : int)"
      
      -- Generate spec (empty for now)
      let spec := "spec " ++ "{\n}"
      
      -- Combine everything
      signature ++ "\n" ++ spec ++ "\n{\n" ++ var_decls ++ body ++ "\n};"
end

-- Translate global variable declarations
def translate_global_vars (stmts: Array Statement) : String :=
  let global_assigns := stmts.filterMap fun stmt =>
    match stmt with
    | Statement.Assign assign =>
        match assign.targets[0]? with
        | some (Expression.Name name) => some s!"var {name.id}: int;"
        | _ => none
    | _ => none
  "\n".intercalate global_assigns.toList

-- Main translation function
def translate_py_to_boogie (m: Module) : String :=
  let header := "program Boogie;\n"
  let global_vars := translate_global_vars m.body
  let functions := m.body.filterMap fun stmt =>
    match stmt with
    | Statement.FunctionDef funcdef => some (translate_function funcdef)
    | _ => none
  let functions_str := "\n\n".intercalate functions.toList
  
  let parts := [header, global_vars, functions_str].filter (· ≠ "")
  "\n\n".intercalate parts
