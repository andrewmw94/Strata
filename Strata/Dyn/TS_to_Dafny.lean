-- For now, `number` is the only TS type and it goes to Dafny `Number`
def translate_ty(_ret_ty: TSTypeAnnotation) : String := "TSNumberType"

def translate_id(id: Identifier) : String :=
  match id.name with
  | "undefined" => "TSNumberType.Undefined"
  | _ => id.name

def translate_arg(arg: Parameter) : String :=
  let name := arg.name
  let ty := translate_ty arg.typeAnnotation
  s!"{name} : {ty}"

def translate_args(args: Array Parameter) : String :=
  ", ".intercalate (args.map translate_arg).toList

def translate_op (op : String) (lhs: String) (rhs: String) : String :=
  match op with
  | "<" => s!"Num_lt({lhs}, {rhs})"
  | "<=" => s!"Num_le({lhs}, {rhs})"
  | ">" => s!"Num_gt({lhs}, {rhs})"
  | ">=" => s!"Num_ge({lhs}, {rhs})"
  | "==" => s!"Num_eq({lhs}, {rhs})"
  | _ => s!"{lhs} {op} {rhs}"

def translate_binary_expr(e: BinaryExpression) : String :=
  let lhs := translate_id e.left
  let rhs := translate_id e.right
  translate_op (e.operator) lhs rhs

mutual
  partial def translate_logical_expr(e: LogicalExpression) : String :=
    let lhs := translate_expr e.left
    let rhs := translate_expr e.right
    translate_op e.operator lhs rhs

  partial def translate_conditional_expr(e: ConditionalExpression) : String :=
    s!"if ({translate_expr e.test}) then {translate_id e.consequent} else {translate_id e.alternate}"

  partial def translate_call_expr(e: CallExpression) : String :=
    let args := ", ".intercalate (e.arguments.map translate_expr)
    s!"{translate_id e.callee} ({args})"

  partial def translate_expr(e: Expression) : String :=
    match e with
    | .BinaryExpression e => translate_binary_expr e
    | .ConditionalExpression e => translate_conditional_expr e
    | .CallExpression e => translate_call_expr e
    | .LogicalExpression e => translate_logical_expr e
end

-- TODO consequent and alternate
def translate_if_statement(ite: IfStatement) : String :=
  let guard := translate_expr ite.test
  let consequent := translate_expr ite.test
  let alternate := translate_expr ite.test
  s!"if ({guard}) then {consequent} else {alternate}"

def translate_ret_statement(ret: ReturnStatement) : String :=
  translate_id ret.argument

def translate_var_declarator(var_decl: VariableDeclarator) : String :=
  let id := translate_id var_decl.id
  let expr := translate_expr var_decl.init
  s!"var {id} := {expr}"

def translate_var_declaration(var_decl: VariableDeclaration) : String :=
  "\n".intercalate (var_decl.declarations.map translate_var_declarator).toList

partial def translate_statement(s: Statement) : String :=
  match s with
  | .IfStatement ite => translate_if_statement ite
  | .ReturnStatement ret => translate_ret_statement ret
  | .VariableDeclaration var_decl => translate_var_declaration var_decl
  | .ExpressionStatement expr_stmt => translate_expr expr_stmt.expression
  | .BlockStatement block => ";\n".intercalate (block.body.map translate_statement).toList


def translate_function(f: FunctionDeclaration) : String :=
  let id := translate_id f.id
  if id == "assert" then
    ""
  else if id == "assume" then
    ""
  else
    let args := translate_args f.params
    let ret_ty := translate_ty f.returnType
    let body := translate_statement f.body
    let s0 := s!"function {id}({args}) : {ret_ty}\n\{\n"
    let s1 := s!"{body}\n}\n"
    "".intercalate [s0,s1]

def translate_program(p: Program) : String :=
  "".intercalate (p.body.map translate_function).toList

def File.to_dafny (ts: File) : String :=
  let program_str := translate_program ts.program
  s!"{TypescriptNumbers}\n{program_str}"
